;;; keepassxc.el --- Interact with the KeePassXC password manager -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/keepassxc.el
;; Keywords: tools processes password keepass keepassxc convenience
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (sodium "0.2"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; keepassxc.el lets Emacs talk to a running KeePassXC instance:
;;
;; - Over the KeePassXC-Browser protocol on a unix domain socket,
;;   end-to-end encrypted with libsodium `crypto_box' (provided by the
;;   sodium.el dynamic module).  This powers `keepassxc-get-logins',
;;   `keepassxc-copy-password', `keepassxc-copy-totp',
;;   `keepassxc-generate-password', entry creation and more.
;;
;; - As an `auth-source' backend (see keepassxc-auth-source.el), so
;;   packages like Gnus, ERC or Forge can read and create credentials
;;   in your KeePassXC database.
;;
;; - Over the KeePassXC D-Bus interface (Linux only) to open, lock and
;;   close databases.
;;
;; Setup: enable "Browser Integration" in the KeePassXC settings, then
;; run M-x keepassxc-associate and confirm the dialog KeePassXC shows.
;; The association is stored GPG-encrypted in a plstore file
;; (see `keepassxc-association-file').
;;
;; All commands are also reachable through the `keepassxc' transient
;; menu.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'browse-url)
(require 'plstore)
(require 'sodium)
(require 'transient)
(eval-when-compile (require 'subr-x))

(declare-function dbus-call-method "dbus")


;;; Customize

(defgroup keepassxc nil
  "KeePassXC password manager integration."
  :prefix "keepassxc-"
  :group 'tools)

(defcustom keepassxc-command "keepassxc"
  "Filename of the keepassxc executable."
  :type 'string)

(defcustom keepassxc-database-file nil
  "Default KeePassXC database file."
  :type '(choice (const :tag "None" nil) file))

(defcustom keepassxc-key-file nil
  "Default KeePassXC database key file."
  :type '(choice (const :tag "None" nil) file))

(defcustom keepassxc-client-id "emacs-keepassxc"
  "Client ID sent to KeePassXC with every message."
  :type 'string)

(defcustom keepassxc-socket-path nil
  "Path of the KeePassXC browser-integration socket.
When nil, the socket is looked up in the standard locations for
Linux (XDG runtime dir, also flatpak and snap layouts) and macOS
\(TMPDIR).  See `keepassxc--socket-candidates'."
  :type '(choice (const :tag "Auto-detect" nil) file))

(defcustom keepassxc-association-file
  (locate-user-emacs-file "keepassxc-associations.plist")
  "Plstore file holding KeePassXC associations per database.
The association key is stored in the encrypted part of the
plstore.  Customize `plstore-encrypt-to' to select the GPG key
used for encryption."
  :type 'file)

(defcustom keepassxc-default-url-schema "https://"
  "URL schema prefixed to URLs that don't specify one."
  :type 'string)

(defcustom keepassxc-timeout 5
  "Seconds to wait for a KeePassXC reply."
  :type 'number)

(defcustom keepassxc-user-interaction-timeout 120
  "Seconds to wait for replies that require user interaction.
Used for association requests, the password generator and
similar actions where KeePassXC shows a dialog."
  :type 'number)

(defcustom keepassxc-trigger-unlock t
  "When non-nil, ask KeePassXC to show the unlock dialog if locked."
  :type 'boolean)

(defcustom keepassxc-password-timeout 45
  "Seconds after which copied secrets are removed from the `kill-ring'.
Set to nil to never clear copied passwords."
  :type '(choice (const :tag "Never clear" nil) number))

(defvar keepassxc-signal-functions nil
  "Abnormal hook run for unsolicited KeePassXC messages.
Each function is called with two arguments: the action string
\(for example \"database-locked\" or \"database-unlocked\") and
the parsed message as a hash-table.")


;;; Errors

(define-error 'keepassxc-error "KeePassXC error")
(define-error 'keepassxc-connection-error "KeePassXC connection error"
              'keepassxc-error)
(define-error 'keepassxc-protocol-error "KeePassXC protocol error"
              'keepassxc-error)
(define-error 'keepassxc-database-locked "KeePassXC database is locked"
              'keepassxc-error)
(define-error 'keepassxc-not-associated "Not associated with KeePassXC"
              'keepassxc-error)
(define-error 'keepassxc-denied "KeePassXC request denied"
              'keepassxc-error)
(define-error 'keepassxc-no-logins "No logins found in KeePassXC"
              'keepassxc-error)

(defconst keepassxc--error-conditions
  '((1 . keepassxc-database-locked)     ; ERROR_KEEPASS_DATABASE_NOT_OPENED
    (6 . keepassxc-denied)              ; ERROR_KEEPASS_ACTION_CANCELLED_OR_DENIED
    (8 . keepassxc-not-associated)      ; ERROR_KEEPASS_ASSOCIATION_FAILED
    (10 . keepassxc-not-associated)     ; ERROR_KEEPASS_ENCRYPTION_KEY_UNRECOGNIZED
    (15 . keepassxc-no-logins)          ; ERROR_KEEPASS_NO_LOGINS_FOUND
    (19 . keepassxc-denied))            ; ERROR_KEEPASS_ACCESS_TO_ALL_ENTRIES_DENIED
  "Alist mapping KeePassXC errorCode values to error symbols.")

(defun keepassxc--error-from-message (msg)
  "Return a (SYMBOL . DATA) error cons for the protocol error MSG."
  (let* ((code (string-to-number (format "%s" (gethash "errorCode" msg 0))))
         (text (or (gethash "error" msg) "Unknown error"))
         (symbol (alist-get code keepassxc--error-conditions 'keepassxc-error)))
    (cons symbol (list text code))))


;;; Session

(cl-defstruct (keepassxc--session (:constructor keepassxc--session-create)
                                  (:copier nil))
  "State of one connection to KeePassXC."
  process         ; network process, or nil when disconnected
  buffer          ; hidden buffer accumulating partial server output
  keypair         ; ephemeral transport keypair, alist with `pk' and `sk'
  server-key      ; server public key for this connection
  db-hash         ; hash of the most recently seen open database
  id              ; association id for the current database
  id-key          ; association public key for the current database
  id-db-hash      ; database hash the association was verified against
  pending-action  ; action string of the in-flight request
  pending-nonce   ; expected (incremented) nonce of the reply
  stale-nonces    ; (NONCE . ACTION) of timed-out requests, to drop late replies
  response        ; parsed reply, set by the process filter
  error)          ; (SYMBOL . DATA) error captured by the process filter

(defvar keepassxc--session nil
  "Default KeePassXC session used by all interactive commands.")

(defun keepassxc--default-session ()
  "Return the default session, creating it if necessary."
  (or keepassxc--session
      (setq keepassxc--session (keepassxc--session-create))))


;;; Socket discovery

(defconst keepassxc--socket-name "org.keepassxc.KeePassXC.BrowserServer"
  "Filename of the KeePassXC unix domain socket.")

(defun keepassxc--socket-candidates ()
  "Return candidate paths for the KeePassXC browser socket.
Ordered by preference; matches the lookup logic of KeePassXC's
`BrowserShared::localServerPath'."
  (let ((runtime-dir (getenv "XDG_RUNTIME_DIR"))
        (snap-dir (getenv "SNAP_USER_COMMON")))
    (delq nil
          (list
           ;; KeePassXC >= 2.7.5 on Linux (also flatpak).
           (when runtime-dir
             (expand-file-name
              (concat "app/org.keepassxc.KeePassXC/" keepassxc--socket-name)
              runtime-dir))
           ;; Legacy symlink kept for backwards compatibility.
           (when runtime-dir
             (expand-file-name keepassxc--socket-name runtime-dir))
           ;; Snap package.
           (when snap-dir
             (expand-file-name keepassxc--socket-name snap-dir))
           ;; macOS and other systems use TMPDIR.
           (expand-file-name keepassxc--socket-name
                             (temporary-file-directory))))))

(defun keepassxc--socket-path ()
  "Return the path of the KeePassXC socket or signal an error."
  (or keepassxc-socket-path
      (seq-find #'file-exists-p (keepassxc--socket-candidates))
      (signal 'keepassxc-connection-error
              (list (concat "Cannot locate the KeePassXC socket; is KeePassXC"
                            " running with browser integration enabled?")))))


;;; Transport

(defconst keepassxc--max-message-size (* 16 1024 1024)
  "Sanity limit for the size of one server message.
Large databases can produce multi-megabyte replies (the 1MiB
NATIVEMSG_MAX_LENGTH limit only applies to the browser
native-messaging proxy, not to the direct socket).")

(defun keepassxc--connect (session)
  "Connect SESSION to KeePassXC and exchange public keys."
  (let ((socket (keepassxc--socket-path))
        (buffer (generate-new-buffer " *keepassxc*" t))
        proc)
    (condition-case err
        (setq proc (make-network-process
                    :name "keepassxc"
                    :family 'local
                    :service socket
                    :coding 'utf-8-unix
                    :filter #'keepassxc--filter
                    :sentinel #'keepassxc--sentinel
                    :noquery t))
      (error
       (kill-buffer buffer)
       (signal 'keepassxc-connection-error
               (list (format "Cannot connect to %s: %s"
                             socket (error-message-string err))))))
    (process-put proc 'keepassxc-session session)
    (setf (keepassxc--session-process session) proc
          (keepassxc--session-buffer session) buffer
          (keepassxc--session-keypair session) (sodium-box-keypair)
          (keepassxc--session-server-key session) nil
          (keepassxc--session-db-hash session) nil)
    ;; A failed handshake must not leave a live but unusable
    ;; connection behind.
    (condition-case err
        (keepassxc--handshake session)
      (error
       (when (process-live-p proc)
         (delete-process proc))
       (keepassxc--cleanup-session session)
       (signal (car err) (cdr err))))
    session))

(defun keepassxc--cleanup-session (session)
  "Clear SESSION's connection state and kill its buffer."
  (when (buffer-live-p (keepassxc--session-buffer session))
    (kill-buffer (keepassxc--session-buffer session)))
  (setf (keepassxc--session-process session) nil
        (keepassxc--session-buffer session) nil
        (keepassxc--session-server-key session) nil
        (keepassxc--session-db-hash session) nil))

(defun keepassxc--sentinel (proc _event)
  "Clean up after PROC's connection state changed."
  (unless (process-live-p proc)
    (when-let* ((session (process-get proc 'keepassxc-session)))
      (when (eq (keepassxc--session-process session) proc)
        (keepassxc--cleanup-session session)))))

(defun keepassxc--filter (proc chunk)
  "Accumulate CHUNK from PROC and dispatch every complete JSON message."
  (when-let* ((session (process-get proc 'keepassxc-session))
              (buffer (keepassxc--session-buffer session)))
    (when (buffer-live-p buffer)
      (let (messages overflow)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert chunk)
          (if (> (buffer-size) keepassxc--max-message-size)
              ;; The connection is beyond repair (we would misparse
              ;; the tail of the oversized message); tear it down.
              (progn
                (erase-buffer)
                (setq overflow t)
                (setf (keepassxc--session-error session)
                      (cons 'keepassxc-connection-error
                            (list "Server message exceeds maximum size"))))
            ;; A JSON message can only become complete when a "}"
            ;; arrives; skip re-parsing large partial messages on
            ;; every intermediate chunk.
            (when (string-search "}" chunk)
              (goto-char (point-min))
              (catch 'keepassxc--incomplete
                (while (progn (skip-chars-forward " \t\n\r") (not (eobp)))
                  (unless (eq (char-after) ?{)
                    ;; Garbage between messages; re-sync to the next
                    ;; object.
                    (if (search-forward "{" nil t)
                        (forward-char -1)
                      (delete-region (point-min) (point-max))
                      (throw 'keepassxc--incomplete nil)))
                  (let ((start (point)))
                    (condition-case nil
                        (push (json-parse-buffer) messages)
                      (json-end-of-file
                       ;; Partial message; wait for more output.
                       (goto-char start)
                       (throw 'keepassxc--incomplete nil))
                      (json-parse-error
                       ;; Skip the offending character and re-sync.
                       (goto-char (1+ start)))))
                  (delete-region (point-min) (point)))))))
        (if overflow
            (delete-process proc)
          (dolist (msg (nreverse messages))
            (when (hash-table-p msg)
              (keepassxc--dispatch session msg))))))))

(defun keepassxc--dispatch (session msg)
  "Dispatch one parsed server message MSG for SESSION.
Replies to the in-flight request are stored in SESSION;
everything else is passed to `keepassxc-signal-functions'.
Errors are captured in SESSION instead of being signaled, since
this runs inside the process filter."
  (let ((action (gethash "action" msg))
        (nonce (gethash "nonce" msg))
        (error-code (gethash "errorCode" msg))
        (stale (keepassxc--session-stale-nonces session))
        (pending (keepassxc--session-pending-action session)))
    (cond
     ;; Late reply to a request that already timed out: drop it so it
     ;; cannot be attributed to a newer request with the same action.
     ((and nonce (assoc nonce stale))
      (setf (keepassxc--session-stale-nonces session)
            (assoc-delete-all nonce stale)))
     ;; Late nonceless error reply: replies arrive in request order,
     ;; so an error for an action with an abandoned request belongs
     ;; to that request, not to the currently pending one.
     ((and error-code (null nonce) (rassoc action stale))
      (setf (keepassxc--session-stale-nonces session)
            (delq (rassoc action stale) stale)))
     ((and pending (equal action pending))
      (cond
       ;; Unencrypted error reply (sent without a nonce).
       (error-code
        (setf (keepassxc--session-error session)
              (keepassxc--error-from-message msg)))
       ;; The server must reply with our request nonce incremented by
       ;; one; anything else could be a replay.
       ((not (equal nonce (keepassxc--session-pending-nonce session)))
        (setf (keepassxc--session-error session)
              (cons 'keepassxc-protocol-error
                    (list (format "Response nonce mismatch for %s" action)))))
       ;; Encrypted reply.
       ((gethash "message" msg)
        (condition-case err
            (let ((inner (json-parse-string
                          (sodium-box-open
                           (gethash "message" msg) nonce
                           (keepassxc--session-server-key session)
                           (alist-get 'sk (keepassxc--session-keypair session))))))
              (if (equal (gethash "success" inner) "true")
                  (setf (keepassxc--session-response session) inner)
                (setf (keepassxc--session-error session)
                      (keepassxc--error-from-message inner))))
          ((sodium-error json-parse-error)
           (setf (keepassxc--session-error session)
                 (cons 'keepassxc-protocol-error
                       (list (format "Cannot decrypt %s reply: %s"
                                     action (error-message-string err))))))))
       ;; Only the key handshake may be answered in plaintext; an
       ;; unencrypted reply to any other action bypasses the
       ;; crypto_box authentication and must be rejected.
       ((equal action "change-public-keys")
        (setf (keepassxc--session-response session) msg))
       (t
        (setf (keepassxc--session-error session)
              (cons 'keepassxc-protocol-error
                    (list (format "Unencrypted %s reply rejected"
                                  action)))))))
     (t
      (when (equal action "database-locked")
        (setf (keepassxc--session-db-hash session) nil))
      (run-hook-with-args 'keepassxc-signal-functions action msg)))))

(defun keepassxc--send-envelope (session envelope &optional timeout)
  "Send ENVELOPE plist over SESSION and wait for the reply.
ENVELOPE must contain :action and :nonce.  Wait at most TIMEOUT
\(default `keepassxc-timeout') seconds.  Return the parsed reply
or signal the error captured by the filter."
  (when (keepassxc--session-pending-action session)
    (signal 'keepassxc-error
            (list "Another KeePassXC request is already in flight")))
  (let ((proc (keepassxc--session-process session)))
    (setf (keepassxc--session-pending-action session)
          (plist-get envelope :action)
          (keepassxc--session-pending-nonce session)
          (sodium-increment (plist-get envelope :nonce))
          (keepassxc--session-response session) nil
          (keepassxc--session-error session) nil)
    (unwind-protect
        (progn
          (condition-case err
              (process-send-string proc (json-serialize envelope))
            (error
             (signal 'keepassxc-connection-error
                     (list (format "Cannot send to KeePassXC: %s"
                                   (error-message-string err))))))
          (let ((deadline (time-add nil (or timeout keepassxc-timeout))))
            (while (and (not (keepassxc--session-response session))
                        (not (keepassxc--session-error session))
                        (process-live-p proc)
                        (time-less-p nil deadline))
              ;; JUST-THIS-ONE would starve servers living in this
              ;; Emacs (like the test mock); nested requests from
              ;; other process filters are rejected by the pending
              ;; check above.
              (accept-process-output proc 0.2)))
          (let ((err (keepassxc--session-error session))
                (response (keepassxc--session-response session)))
            (cond
             (err (signal (car err) (cdr err)))
             (response response)
             ((not (process-live-p proc))
              (signal 'keepassxc-connection-error
                      (list "Connection to KeePassXC closed")))
             (t
              ;; The reply may still arrive; remember its nonce (and
              ;; action, for nonceless error replies) so the late
              ;; reply is dropped instead of poisoning the next
              ;; request.
              (setf (keepassxc--session-stale-nonces session)
                    (cons (cons (keepassxc--session-pending-nonce session)
                                (plist-get envelope :action))
                          (seq-take (keepassxc--session-stale-nonces session)
                                    7)))
              (signal 'keepassxc-connection-error
                      (list (format "Timeout waiting for KeePassXC (%s)"
                                    (plist-get envelope :action))))))))
      (setf (keepassxc--session-pending-action session) nil
            (keepassxc--session-pending-nonce session) nil
            (keepassxc--session-response session) nil))))

(defun keepassxc--handshake (session)
  "Exchange public keys with KeePassXC for SESSION."
  (let* ((nonce (sodium-box-make-nonce))
         (response (keepassxc--send-envelope
                    session
                    `(:action "change-public-keys"
                      :publicKey ,(alist-get
                                   'pk (keepassxc--session-keypair session))
                      :nonce ,nonce
                      :clientID ,keepassxc-client-id)))
         (server-key (gethash "publicKey" response)))
    (unless server-key
      (signal 'keepassxc-protocol-error
              (list "KeePassXC did not send a public key")))
    (setf (keepassxc--session-server-key session) server-key)))

(defun keepassxc--ensure-connection (session)
  "Connect SESSION to KeePassXC unless it is connected already.
A connection without a completed handshake is torn down and
reopened."
  (let ((proc (keepassxc--session-process session)))
    (unless (and (process-live-p proc)
                 (keepassxc--session-server-key session))
      (when (process-live-p proc)
        (delete-process proc))
      (keepassxc--cleanup-session session)
      (keepassxc--connect session)))
  session)

(cl-defun keepassxc--request (session action &optional msg timeout
                                      (trigger-unlock keepassxc-trigger-unlock))
  "Send encrypted ACTION with plist MSG over SESSION.
Wait TIMEOUT seconds (default `keepassxc-timeout') for the reply
and return the decrypted inner message as a hash-table.
TRIGGER-UNLOCK, when non-nil, asks KeePassXC to prompt the user
to unlock a locked database."
  (keepassxc--ensure-connection session)
  (let* ((nonce (sodium-box-make-nonce))
         (inner (json-serialize (append `(:action ,action) msg)))
         (cipher (sodium-box
                  inner nonce
                  (keepassxc--session-server-key session)
                  (alist-get 'sk (keepassxc--session-keypair session)))))
    (keepassxc--send-envelope
     session
     `(:action ,action
       :message ,cipher
       :nonce ,nonce
       :clientID ,keepassxc-client-id
       :triggerUnlock ,(if trigger-unlock "true" "false"))
     timeout)))


;;; Association persistence

(defvar keepassxc--plstore nil
  "Cached plstore holding the KeePassXC associations.")
(defvar keepassxc--plstore-file nil
  "Filename `keepassxc--plstore' was opened from.")

(defun keepassxc--association-store ()
  "Return the plstore for `keepassxc-association-file'."
  (let ((file (expand-file-name keepassxc-association-file)))
    (unless (and keepassxc--plstore
                 (equal keepassxc--plstore-file file))
      (let ((dir (file-name-directory file)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (setq keepassxc--plstore (plstore-open file)
            keepassxc--plstore-file file))
    keepassxc--plstore))

(defun keepassxc--association-get (db-hash)
  "Return the stored association plist (:id ID :id-key KEY) for DB-HASH."
  (when-let* ((entry (plstore-get (keepassxc--association-store) db-hash)))
    (cdr entry)))

(defun keepassxc--association-put (db-hash id id-key)
  "Store the association ID and ID-KEY for DB-HASH."
  (let ((store (keepassxc--association-store)))
    (plstore-put store db-hash `(:id ,id) `(:id-key ,id-key))
    (plstore-save store)))


;;; Association

(defun keepassxc--fetch-db-hash (session)
  "Return the hash of the database currently open in SESSION.
Always queries KeePassXC: the user can switch the active database
at any time without Emacs receiving a signal."
  (setf (keepassxc--session-db-hash session)
        (gethash "hash" (keepassxc--request session "get-databasehash"))))

(defun keepassxc--test-associate (session)
  "Return non-nil if SESSION's id and id-key are a valid association."
  (and (keepassxc--session-id session)
       (keepassxc--session-id-key session)
       (condition-case nil
           (progn
             (keepassxc--request session "test-associate"
                                 `(:id ,(keepassxc--session-id session)
                                   :key ,(keepassxc--session-id-key session)))
             t)
         (keepassxc-not-associated nil))))

(defun keepassxc--associate (session)
  "Create a new association for SESSION.
KeePassXC asks the user to confirm and name the association.
Return the new association id."
  (keepassxc--ensure-connection session)
  (let* ((id-key (alist-get 'pk (sodium-box-keypair)))
         (response (keepassxc--request
                    session "associate"
                    `(:key ,(alist-get 'pk (keepassxc--session-keypair session))
                      :idKey ,id-key)
                    keepassxc-user-interaction-timeout))
         (id (gethash "id" response))
         (db-hash (or (gethash "hash" response)
                      (keepassxc--fetch-db-hash session))))
    (when (or (null id) (string-empty-p id))
      (signal 'keepassxc-not-associated
              (list "KeePassXC did not return an association id")))
    (setf (keepassxc--session-id session) id
          (keepassxc--session-id-key session) id-key
          (keepassxc--session-id-db-hash session) db-hash)
    (keepassxc--association-put db-hash id id-key)
    id))

(defun keepassxc--ensure-association (session)
  "Ensure SESSION is associated with the currently open database.
Load and verify the stored association or create a new one
\(KeePassXC shows a confirmation dialog).  Return the
association id."
  (keepassxc--ensure-connection session)
  (let ((db-hash (keepassxc--fetch-db-hash session)))
    (if (equal db-hash (keepassxc--session-id-db-hash session))
        (keepassxc--session-id session)
      (let ((stored (keepassxc--association-get db-hash)))
        (setf (keepassxc--session-id session) (plist-get stored :id)
              (keepassxc--session-id-key session) (plist-get stored :id-key)
              (keepassxc--session-id-db-hash session) nil)
        (if (and stored (keepassxc--test-associate session))
            (setf (keepassxc--session-id-db-hash session) db-hash)
          (keepassxc--associate session))
        (keepassxc--session-id session)))))


;;; Protocol actions

(defun keepassxc-get-database-hash ()
  "Return the hash of the currently open KeePassXC database."
  (keepassxc--fetch-db-hash
   (keepassxc--ensure-connection (keepassxc--default-session))))

;;;###autoload
(defun keepassxc-associate ()
  "Associate Emacs with KeePassXC.
KeePassXC shows a dialog asking to confirm and name the new
association.  The association is stored encrypted in
`keepassxc-association-file'."
  (interactive)
  (let ((id (keepassxc--associate (keepassxc--default-session))))
    (message "Associated with KeePassXC as %S" id)
    id))

(defun keepassxc-test-associate ()
  "Return the association id if the current session is associated."
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-connection session)
    (and (keepassxc--test-associate session)
         (keepassxc--session-id session))))

;;;###autoload
(defun keepassxc-get-logins (url &optional submit-url http-auth)
  "Return a list of KeePassXC entries matching URL.
Each entry is a hash-table with keys like \"login\",
\"password\", \"name\", \"uuid\", \"group\" and optionally
\"totp\" and \"stringFields\".  SUBMIT-URL restricts matches to a
form-submit URL; HTTP-AUTH non-nil requests HTTP-auth entries.
Signal `keepassxc-no-logins' when nothing matches."
  (interactive (list (keepassxc--read-url)))
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (let* ((id (keepassxc--session-id session))
           (id-key (keepassxc--session-id-key session))
           (response (keepassxc--request
                      session "get-logins"
                      `(:id ,id
                        :url ,url
                        ,@(when submit-url `(:submitUrl ,submit-url))
                        ,@(when http-auth '(:httpAuth "true"))
                        :keys ,(vector (list :id id :key id-key)))))
           (entries (append (gethash "entries" response) nil)))
      (when (called-interactively-p 'interactive)
        (message "%d KeePassXC entries for %s" (length entries) url))
      entries)))

;;;###autoload
(defun keepassxc-set-login (url login password &optional uuid group group-uuid)
  "Create or update the KeePassXC entry for URL with LOGIN and PASSWORD.
With UUID, update that existing entry instead of creating a new
one.  GROUP and GROUP-UUID select the group for new entries."
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (keepassxc--request
     session "set-login"
     `(:id ,(keepassxc--session-id session)
       :url ,url
       :submitUrl ,url
       :login ,login
       :password ,password
       ,@(when uuid `(:uuid ,uuid))
       ,@(when group `(:group ,group))
       ,@(when group-uuid `(:groupUuid ,group-uuid))))))

(defun keepassxc-get-database-groups ()
  "Return the group tree of the open KeePassXC database."
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (gethash "groups" (keepassxc--request session "get-database-groups"))))

;;;###autoload
(defun keepassxc-create-new-group (name)
  "Create the group NAME in the KeePassXC database.
Use \"/\" in NAME to create nested groups, e.g. \"emacs/mail\"."
  (interactive "sNew KeePassXC group (use / for nesting): ")
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (keepassxc--request session "create-new-group" `(:groupName ,name)
                        keepassxc-user-interaction-timeout)))

(defun keepassxc-get-totp (uuid)
  "Return the current TOTP for the KeePassXC entry UUID."
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (let ((totp (gethash "totp" (keepassxc--request session "get-totp"
                                                    `(:uuid ,uuid)))))
      (if (and totp (not (string-empty-p totp)))
          totp
        (user-error "Entry has no TOTP configured")))))

;;;###autoload
(defun keepassxc-delete-entry (uuid)
  "Move the KeePassXC entry UUID to the recycle bin."
  (interactive
   (let ((entry (keepassxc--read-entry
                 (keepassxc--read-url "Delete entry for URL: "))))
     (unless (yes-or-no-p (format "Delete KeePassXC entry %S? "
                                  (gethash "name" entry)))
       (user-error "Aborted"))
     (list (gethash "uuid" entry))))
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (keepassxc--request session "delete-entry" `(:uuid ,uuid))
    (message "Entry deleted")))

;;;###autoload
(defun keepassxc-request-autotype (search)
  "Ask KeePassXC to perform global auto-type for SEARCH.
SEARCH is matched against entry URLs (usually a domain name)."
  (interactive "sAuto-type search term (domain): ")
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-connection session)
    (keepassxc--request session "request-autotype" `(:search ,search)
                        keepassxc-user-interaction-timeout)))

(defun keepassxc-get-database-entries ()
  "Return all entries of the open KeePassXC database.
Requires \"Allow limited access to all entries\" (get database
entries requests) to be enabled in the KeePassXC browser
settings."
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-association session)
    (append (gethash "entries"
                     (keepassxc--request session "get-database-entries"))
            nil)))

;;;###autoload
(defun keepassxc-generate-password ()
  "Generate a password with the KeePassXC password generator.
KeePassXC opens its generator dialog; the password is returned
once you accept it there.  Interactively, also copy it to the
`kill-ring' (cleared after `keepassxc-password-timeout' seconds)."
  (interactive)
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-connection session)
    (let* ((response (keepassxc--request
                      session "generate-password"
                      `(:requestID ,(sodium-box-make-nonce))
                      keepassxc-user-interaction-timeout))
           (password
            (or (gethash "password" response)
                ;; KeePassXC < 2.7 returned entries[0].password.
                (when-let* ((entries (gethash "entries" response))
                            ((> (length entries) 0)))
                  (gethash "password" (aref entries 0))))))
      (unless password
        (signal 'keepassxc-protocol-error
                (list "No password in generate-password reply")))
      (when (called-interactively-p 'interactive)
        (keepassxc--kill-secret password "Generated password"))
      password)))

;;;###autoload
(defun keepassxc-lock-database ()
  "Lock the currently open KeePassXC database."
  (interactive)
  (let ((session (keepassxc--default-session)))
    (keepassxc--ensure-connection session)
    (keepassxc--request session "lock-database" nil nil nil)
    (message "KeePassXC database locked")))

(defun keepassxc-disconnect ()
  "Close the connection to KeePassXC."
  (interactive)
  (when-let* ((session keepassxc--session)
              (proc (keepassxc--session-process session)))
    (when (process-live-p proc)
      (delete-process proc))
    (keepassxc--cleanup-session session))
  (message "Disconnected from KeePassXC"))


;;; Entry selection and copy commands

(defvar keepassxc--url-history nil
  "Minibuffer history for KeePassXC URL prompts.")

(defun keepassxc--read-url (&optional prompt)
  "Read a URL with PROMPT, prefixing `keepassxc-default-url-schema'.
The URL at point (if any) is used as default."
  (let ((url (car (browse-url-interactive-arg (or prompt "URL: ")))))
    (if (string-match-p "://" url)
        url
      (concat keepassxc-default-url-schema url))))

(defun keepassxc--entry-candidates (entries)
  "Return an alist of (LABEL . ENTRY) with unique labels for ENTRIES."
  (let (result)
    (dolist (entry entries)
      (let* ((name (or (gethash "name" entry) "?"))
             (login (gethash "login" entry))
             (base (if (and login (not (string-empty-p login)))
                       (format "%s (%s)" name login)
                     name))
             (label base)
             (n 2))
        (while (assoc label result)
          (setq label (format "%s <%d>" base n)
                n (1+ n)))
        (push (cons label entry) result)))
    (nreverse result)))

(defun keepassxc--completion-table (candidates)
  "Return a completion table over CANDIDATES, an alist (LABEL . ENTRY)."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . keepassxc-entry)
          (annotation-function
           . ,(lambda (cand)
                (when-let* ((entry (cdr (assoc cand candidates))))
                  (propertize
                   (concat "  " (or (gethash "group" entry) "")
                           (when (gethash "totp" entry) "  [totp]"))
                   'face 'completions-annotations)))))
      (complete-with-action action candidates string pred))))

(defun keepassxc--read-entry (url &optional prompt)
  "Select a KeePassXC entry for URL with completion.
PROMPT overrides the default minibuffer prompt.  Return the
entry hash-table; don't prompt when only one entry matches."
  (let ((entries (condition-case nil
                     (keepassxc-get-logins url)
                   (keepassxc-no-logins nil))))
    (cond
     ((null entries) (user-error "No KeePassXC entries for %s" url))
     ((null (cdr entries)) (car entries))
     (t (let* ((candidates (keepassxc--entry-candidates entries))
               (choice (completing-read
                        (or prompt (format "Entry for %s: " url))
                        (keepassxc--completion-table candidates)
                        nil t)))
          (cdr (assoc choice candidates)))))))

(defvar keepassxc--clear-timer nil
  "Timer clearing the last copied secret from the `kill-ring'.")

(defvar keepassxc--pending-secret nil
  "Cons (SECRET . WHAT) of the last copied, not yet cleared secret.")

(defun keepassxc--clear-pending-secret (&optional quiet)
  "Remove the pending copied secret from the `kill-ring'.
With QUIET non-nil, don't message and don't touch the system
clipboard; used right before copying a new secret."
  (when keepassxc--clear-timer
    (cancel-timer keepassxc--clear-timer)
    (setq keepassxc--clear-timer nil))
  (when keepassxc--pending-secret
    (let ((secret (car keepassxc--pending-secret))
          (what (cdr keepassxc--pending-secret)))
      (setq keepassxc--pending-secret nil)
      (when (and (not quiet)
                 (equal (ignore-errors (current-kill 0 t)) secret))
        ;; Also replaces the system clipboard.
        (kill-new ""))
      (setq kill-ring (delete secret kill-ring)
            kill-ring-yank-pointer kill-ring)
      (unless quiet
        (message "%s cleared from kill-ring" what)))))

(defun keepassxc--kill-secret (secret what)
  "Copy SECRET (described by WHAT) to the `kill-ring'.
The secret is removed again after `keepassxc-password-timeout'
seconds.  A previously copied, not yet cleared secret is removed
immediately."
  (keepassxc--clear-pending-secret t)
  (kill-new secret)
  (if (not keepassxc-password-timeout)
      (message "%s copied to kill-ring" what)
    (setq keepassxc--pending-secret (cons secret what)
          keepassxc--clear-timer
          (run-at-time keepassxc-password-timeout nil
                       #'keepassxc--clear-pending-secret))
    (message "%s copied to kill-ring (clears in %ss)"
             what keepassxc-password-timeout)))

;;;###autoload
(defun keepassxc-get-login (url)
  "Return a KeePassXC entry for URL, selecting one when several match.
Interactively, copy the username and then the password to the
`kill-ring', so the password is the most recent kill; the password
is cleared after `keepassxc-password-timeout' seconds."
  (interactive (list (keepassxc--read-url)))
  (let ((entry (keepassxc--read-entry url)))
    (when (called-interactively-p 'interactive)
      (let ((login (gethash "login" entry)))
        (when (and login (not (string-empty-p login)))
          (kill-new login)))
      (keepassxc--kill-secret
       (gethash "password" entry)
       (format "Username and password for %S -- password" (gethash "name" entry))))
    entry))

;;;###autoload
(defun keepassxc-copy-password (url)
  "Copy the password of a KeePassXC entry for URL to the `kill-ring'.
The password is cleared after `keepassxc-password-timeout' seconds."
  (interactive (list (keepassxc--read-url)))
  (let ((entry (keepassxc--read-entry url)))
    (keepassxc--kill-secret (gethash "password" entry)
                            (format "Password for %S" (gethash "name" entry)))))

;;;###autoload
(defun keepassxc-copy-username (url)
  "Copy the username of a KeePassXC entry for URL to the `kill-ring'."
  (interactive (list (keepassxc--read-url)))
  (let* ((entry (keepassxc--read-entry url))
         (login (gethash "login" entry)))
    (if (and login (not (string-empty-p login)))
        (progn
          (kill-new login)
          (message "Username for %S copied to kill-ring"
                   (gethash "name" entry)))
      (user-error "Entry %S has no username" (gethash "name" entry)))))

;;;###autoload
(defun keepassxc-copy-totp (url)
  "Copy the current TOTP of a KeePassXC entry for URL to the `kill-ring'.
The TOTP is cleared after `keepassxc-password-timeout' seconds."
  (interactive (list (keepassxc--read-url)))
  (let* ((entry (keepassxc--read-entry url))
         (inline-totp (gethash "totp" entry))
         (totp (if (and inline-totp (not (string-empty-p inline-totp)))
                   inline-totp
                 (keepassxc-get-totp (gethash "uuid" entry)))))
    (keepassxc--kill-secret totp
                            (format "TOTP for %S" (gethash "name" entry)))))

;;;###autoload
(defun keepassxc-create-login (url login password)
  "Create a new KeePassXC entry for URL with LOGIN and PASSWORD.
Interactively, offer to generate the password with the KeePassXC
password generator."
  (interactive
   (let* ((url (keepassxc--read-url "New entry URL: "))
          (login (read-string "Username: "))
          (password (if (y-or-n-p "Generate password with KeePassXC? ")
                        (keepassxc-generate-password)
                      (read-passwd "Password: " t))))
     (list url login password)))
  (keepassxc-set-login url login password)
  (message "KeePassXC entry for %s created" url))


;;; D-Bus interface (Linux only)

(defconst keepassxc--dbus-service "org.keepassxc.KeePassXC.MainWindow")
(defconst keepassxc--dbus-path "/keepassxc")
(defconst keepassxc--dbus-interface "org.keepassxc.KeePassXC.MainWindow")

(defun keepassxc--dbus-available-p ()
  "Return non-nil when the KeePassXC D-Bus interface is usable."
  (and (featurep 'dbusbind)
       (getenv "DBUS_SESSION_BUS_ADDRESS")
       (require 'dbus nil t)
       t))

(defun keepassxc--call-dbus-method (method &rest args)
  "Call the KeePassXC D-Bus METHOD with ARGS."
  (unless (keepassxc--dbus-available-p)
    (user-error "The KeePassXC D-Bus interface needs a D-Bus session (Linux)"))
  (apply #'dbus-call-method :session keepassxc--dbus-service
         keepassxc--dbus-path keepassxc--dbus-interface method args))

;;;###autoload
(defun keepassxc-open ()
  "Start KeePassXC.
`keepassxc-command' is interpreted by the shell, so it may
contain arguments (e.g. \"flatpak run org.keepassxc.KeePassXC\")."
  (interactive)
  (start-process-shell-command "keepassxc" nil keepassxc-command))

;;;###autoload
(defun keepassxc-lock-all-databases ()
  "Lock all opened KeePassXC databases."
  (interactive)
  (keepassxc--call-dbus-method "lockAllDatabases"))

;;;###autoload
(defun keepassxc-close-all-databases ()
  "Close all opened KeePassXC databases."
  (interactive)
  (keepassxc--call-dbus-method "closeAllDatabases"))

;;;###autoload
(defun keepassxc-exit ()
  "Quit KeePassXC."
  (interactive)
  (keepassxc--call-dbus-method "appExit"))

(defun keepassxc--read-database-file ()
  "Return `keepassxc-database-file' or prompt for a database."
  (or keepassxc-database-file
      (read-file-name "Database: " nil nil t)))

;;;###autoload
(defun keepassxc-open-database (db)
  "Open the KeePassXC database DB.
Interactively, use `keepassxc-database-file' without prompting
when it is set."
  (interactive (list (keepassxc--read-database-file)))
  (keepassxc--call-dbus-method "openDatabase" (expand-file-name db)))

;;;###autoload
(defun keepassxc-open-database-password (db password)
  "Open the KeePassXC database DB with PASSWORD."
  (interactive
   (list (keepassxc--read-database-file)
         (read-passwd "Password: ")))
  (keepassxc--call-dbus-method "openDatabase" (expand-file-name db) password))

;;;###autoload
(defun keepassxc-open-database-password-key (db password key-file)
  "Open the KeePassXC database DB with PASSWORD and KEY-FILE."
  (interactive
   (list (keepassxc--read-database-file)
         (read-passwd "Password: ")
         (or keepassxc-key-file (read-file-name "Key file: " nil nil t))))
  (keepassxc--call-dbus-method "openDatabase" (expand-file-name db)
                               password (expand-file-name key-file)))

(defun keepassxc-hardware-key-supported-p ()
  "Return non-nil if KeePassXC supports hardware keys."
  (interactive)
  (let ((supported (keepassxc--call-dbus-method "isHardwareKeySupported")))
    (when (called-interactively-p 'interactive)
      (message "Hardware keys are %ssupported" (if supported "" "not ")))
    supported))

;;;###autoload
(defun keepassxc-refresh-hardware-keys ()
  "Refresh the list of hardware keys in KeePassXC."
  (interactive)
  (keepassxc--call-dbus-method "refreshHardwareKeys"))


;;; Transient menu

;;;###autoload (autoload 'keepassxc "keepassxc" "KeePassXC transient menu." t)
(transient-define-prefix keepassxc ()
  "KeePassXC commands."
  [["Entry"
    ("p" "Copy password" keepassxc-copy-password)
    ("u" "Copy username" keepassxc-copy-username)
    ("t" "Copy TOTP" keepassxc-copy-totp)
    ("y" "Auto-type" keepassxc-request-autotype)
    ("n" "New entry" keepassxc-create-login)
    ("D" "Delete entry" keepassxc-delete-entry)]
   ["Database"
    ("g" "Generate password" keepassxc-generate-password)
    ("N" "New group" keepassxc-create-new-group)
    ("l" "Lock database" keepassxc-lock-database)
    ("a" "Associate" keepassxc-associate)
    ("d" "Disconnect" keepassxc-disconnect)]
   ["Application"
    ("o" "Open KeePassXC" keepassxc-open)
    ("O" "Open database" keepassxc-open-database
     :if keepassxc--dbus-available-p)
    ("L" "Lock all databases" keepassxc-lock-all-databases
     :if keepassxc--dbus-available-p)
    ("C" "Close all databases" keepassxc-close-all-databases
     :if keepassxc--dbus-available-p)
    ("Q" "Quit KeePassXC" keepassxc-exit
     :if keepassxc--dbus-available-p)]])

(provide 'keepassxc)
;;; keepassxc.el ends here
