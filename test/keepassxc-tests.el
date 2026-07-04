;;; keepassxc-tests.el --- Tests for keepassxc.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Kraus <daniel@kraus.my>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for keepassxc.el and keepassxc-auth-source.el, running
;; against the in-Emacs mock server from keepassxc-mock-server.el.
;; The crypto is real (sodium module required); only the KeePassXC
;; process is simulated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'keepassxc)
(require 'keepassxc-auth-source)
(require 'keepassxc-mock-server)

(defvar keepassxc-tests--entries
  '(("https://example.com"
     . ((:login "alice" :password "s3cret" :name "Example"
         :uuid "uuid-1" :group "Web")
        (:login "bob" :password "hunter2" :name "Example"
         :uuid "uuid-2" :group "Web" :totp "999111")))
    ("imaps://mail.example.com"
     . ((:login "carol" :password "imap-pass" :name "Mail"
         :uuid "uuid-3" :group "Mail")))
    ("https://empty.example.com"
     . ((:login "" :password "anon-pass" :name "Anon"
         :uuid "uuid-4" :group "Web")
        (:login "eve" :password "eve-pass" :name "Eve"
         :uuid "uuid-5" :group "Web"))))
  "Canned entries served by the mock server.")

(defvar keepassxc-tests--assocs nil
  "In-memory replacement for the plstore association storage.")

(defmacro keepassxc-tests--with-mock (options &rest body)
  "Run BODY against a mock KeePassXC server, bound to `mock'.
OPTIONS is an expression evaluating to a plist passed to
`keepassxc-mock-start'.  A fresh default session is used and the
plstore persistence is replaced with an in-memory alist."
  (declare (indent 1) (debug t))
  `(let* ((dir (make-temp-file "keepassxc-tests" t))
          (socket (expand-file-name "kpxc.sock" dir))
          (mock (apply #'keepassxc-mock-start socket
                       (append ,options
                               (list :entries keepassxc-tests--entries))))
          (keepassxc-socket-path socket)
          (keepassxc--session nil)
          (keepassxc-tests--assocs nil)
          (keepassxc-password-timeout nil)
          (keepassxc-timeout 5)
          (keepassxc-user-interaction-timeout 5))
     (ignore mock)
     (cl-letf (((symbol-function 'keepassxc--association-get)
                (lambda (hash) (cdr (assoc hash keepassxc-tests--assocs))))
               ((symbol-function 'keepassxc--association-put)
                (lambda (hash id id-key)
                  (setf (alist-get hash keepassxc-tests--assocs
                                   nil nil #'equal)
                        (list :id id :id-key id-key)))))
       (unwind-protect
           (progn ,@body)
         (when-let* ((session keepassxc--session)
                     (proc (keepassxc--session-process session)))
           (when (process-live-p proc)
             (delete-process proc)))
         (keepassxc-mock-stop mock)
         (delete-directory dir t)))))


;;; Pure helpers

(ert-deftest keepassxc-tests-socket-candidates ()
  "Socket discovery covers XDG, legacy, snap and TMPDIR locations."
  (with-environment-variables (("XDG_RUNTIME_DIR" "/run/user/1000")
                               ("SNAP_USER_COMMON" "/snap/common"))
    (let ((candidates (keepassxc--socket-candidates)))
      (should (equal (car candidates)
                     (concat "/run/user/1000/app/org.keepassxc.KeePassXC/"
                             "org.keepassxc.KeePassXC.BrowserServer")))
      (should (member "/run/user/1000/org.keepassxc.KeePassXC.BrowserServer"
                      candidates))
      (should (member "/snap/common/org.keepassxc.KeePassXC.BrowserServer"
                      candidates))
      (should (= (length candidates) 4)))))

(ert-deftest keepassxc-tests-error-mapping ()
  "KeePassXC errorCodes map to the right error symbols."
  (let ((msg (make-hash-table :test #'equal)))
    (puthash "error" "some error" msg)
    (dolist (case '(("1" . keepassxc-database-locked)
                    ("6" . keepassxc-denied)
                    ("8" . keepassxc-not-associated)
                    ("10" . keepassxc-not-associated)
                    ("15" . keepassxc-no-logins)
                    ("42" . keepassxc-error)))
      (puthash "errorCode" (car case) msg)
      (should (eq (car (keepassxc--error-from-message msg)) (cdr case))))))

(ert-deftest keepassxc-tests-auth-source-urls ()
  "Host/port pairs are translated to KeePassXC lookup URLs."
  (should (equal (keepassxc-auth-source--urls "example.com" nil)
                 '("https://example.com")))
  (should (equal (keepassxc-auth-source--urls "example.com" 443)
                 '("https://example.com")))
  (should (equal (keepassxc-auth-source--urls "mail.example.com" 993)
                 '("imaps://mail.example.com" "https://mail.example.com")))
  (should (equal (keepassxc-auth-source--urls "irc.libera.chat" "6697")
                 '("ircs://irc.libera.chat" "https://irc.libera.chat")))
  (should (equal (keepassxc-auth-source--urls "imap.example.com" "imaps")
                 '("imaps://imap.example.com" "https://imap.example.com"))))


;;; Transport and association

(ert-deftest keepassxc-tests-handshake-and-db-hash ()
  "Connecting exchanges keys and get-databasehash works."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (should (keepassxc--session-server-key keepassxc--session))))

(ert-deftest keepassxc-tests-associate-once ()
  "The first request associates; subsequent requests reuse it."
  (keepassxc-tests--with-mock nil
    (should (= (length (keepassxc-get-logins "https://example.com")) 2))
    (keepassxc-get-logins "https://example.com")
    (should (= (length (keepassxc-mock-requests-for mock "inner:associate")) 1))
    (should (equal (plist-get (cdr (assoc "MOCKHASH" keepassxc-tests--assocs))
                              :id)
                   "mock-assoc-0"))))

(ert-deftest keepassxc-tests-reconnect-uses-stored-association ()
  "After a reconnect the stored association is verified, not recreated."
  (keepassxc-tests--with-mock nil
    (keepassxc-get-logins "https://example.com")
    (keepassxc-disconnect)
    (setq keepassxc--session nil)
    (keepassxc-get-logins "https://example.com")
    (should (= (length (keepassxc-mock-requests-for mock "inner:associate")) 1))
    (should (>= (length (keepassxc-mock-requests-for
                         mock "inner:test-associate"))
                1))))

(ert-deftest keepassxc-tests-set-login-sends-association-id ()
  "Regression: set-login must send the association id, not the id-key."
  (keepassxc-tests--with-mock nil
    (keepassxc-get-logins "https://example.com")
    (keepassxc-set-login "https://new.example.com" "dave" "pw")
    (let* ((inner (car (keepassxc-mock-requests-for mock "inner:set-login")))
           (association (car (keepassxc-mock-associations mock))))
      (should inner)
      (should (equal (gethash "id" inner) (car association)))
      (should-not (equal (gethash "id" inner) (cdr association))))))

(ert-deftest keepassxc-tests-get-logins-mapping ()
  "Entries are returned as hash-tables; unknown URLs signal no-logins."
  (keepassxc-tests--with-mock nil
    (let ((entries (keepassxc-get-logins "https://example.com")))
      (should (= (length entries) 2))
      (should (equal (gethash "login" (car entries)) "alice"))
      (should (equal (gethash "password" (car entries)) "s3cret"))
      (should (equal (gethash "totp" (cadr entries)) "999111")))
    (should-error (keepassxc-get-logins "https://unknown.example.com")
                  :type 'keepassxc-no-logins)))

(ert-deftest keepassxc-tests-locked-database ()
  "A locked database signals `keepassxc-database-locked'."
  (keepassxc-tests--with-mock '(:locked t)
    (should-error (keepassxc-get-logins "https://example.com")
                  :type 'keepassxc-database-locked)))

(ert-deftest keepassxc-tests-nonce-mismatch-signaled-to-caller ()
  "Regression: a bad reply nonce signals an error instead of timing out."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (setf (keepassxc-mock-corrupt-nonce mock) t)
    (should-error (keepassxc-get-logins "https://example.com")
                  :type 'keepassxc-protocol-error)))

(ert-deftest keepassxc-tests-split-chunks ()
  "Regression: replies arriving in multiple chunks are reassembled."
  (keepassxc-tests--with-mock nil
    (setf (keepassxc-mock-chunk-mode mock) 'split)
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (should (= (length (keepassxc-get-logins "https://example.com")) 2))))

(ert-deftest keepassxc-tests-coalesced-messages ()
  "Regression: back-to-back messages in one chunk are all handled."
  (keepassxc-tests--with-mock nil
    (let* ((signals nil)
           (keepassxc-signal-functions
            (list (lambda (action _msg) (push action signals)))))
      (setf (keepassxc-mock-chunk-mode mock) 'coalesce)
      (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
      (should (member "database-unlocked" signals)))))

(ert-deftest keepassxc-tests-garbage-resync ()
  "Regression: stray non-JSON bytes on the socket are skipped."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (process-send-string (keepassxc-mock-client-proc mock) " . ")
    (should (= (length (keepassxc-get-logins "https://example.com")) 2))))

(ert-deftest keepassxc-tests-handshake-failure-recovers ()
  "A failed handshake tears the connection down; the next call retries."
  (keepassxc-tests--with-mock nil
    (setf (keepassxc-mock-hold mock) t)
    (let ((keepassxc-timeout 0.3))
      (should-error (keepassxc-get-database-hash)
                    :type 'keepassxc-connection-error))
    ;; No half-initialized connection may linger.
    (should-not (keepassxc--session-process keepassxc--session))
    (setf (keepassxc-mock-hold mock) nil
          (keepassxc-mock-held mock) nil)
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))))

(ert-deftest keepassxc-tests-stale-reply-dropped ()
  "A reply arriving after its request timed out is dropped."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (setf (keepassxc-mock-hold mock) t)
    (let ((keepassxc-timeout 0.3)
          (keepassxc-user-interaction-timeout 0.3))
      (should-error (keepassxc-get-logins "https://example.com")
                    :type 'keepassxc-connection-error))
    (setf (keepassxc-mock-hold mock) nil)
    ;; Deliver the stale reply; it must be silently discarded.
    (keepassxc-mock-flush mock)
    (accept-process-output nil 0.2)
    (should (= (length (keepassxc-get-logins "https://example.com")) 2))))

(ert-deftest keepassxc-tests-unencrypted-reply-rejected ()
  "An unencrypted reply to an encrypted action must be rejected.
Accepting it would bypass the crypto_box authentication."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (setf (keepassxc-mock-plaintext mock) t)
    (should-error (keepassxc-get-database-hash)
                  :type 'keepassxc-protocol-error)))

(ert-deftest keepassxc-tests-stale-error-reply-dropped ()
  "A late nonceless error reply belongs to the abandoned request."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-database-hash) "MOCKHASH"))
    (setf (keepassxc-mock-hold mock) t
          (keepassxc-mock-fail-with mock) 6)
    (let ((keepassxc-timeout 0.3)
          (keepassxc-user-interaction-timeout 0.3))
      (should-error (keepassxc-get-logins "https://example.com")
                    :type 'keepassxc-connection-error))
    (setf (keepassxc-mock-hold mock) nil)
    ;; Deliver the stale error reply; it must not be attributed to
    ;; the retry below.
    (keepassxc-mock-flush mock)
    (accept-process-output nil 0.2)
    (should (= (length (keepassxc-get-logins "https://example.com")) 2))))

(ert-deftest keepassxc-tests-locked-signal-clears-db-hash ()
  "An unsolicited database-locked signal invalidates the cached hash."
  (keepassxc-tests--with-mock nil
    (keepassxc-get-database-hash)
    (should (keepassxc--session-db-hash keepassxc--session))
    (keepassxc-mock-send-signal mock "database-locked")
    (let ((deadline (time-add nil 2)))
      (while (and (keepassxc--session-db-hash keepassxc--session)
                  (time-less-p nil deadline))
        (accept-process-output nil 0.05)))
    (should-not (keepassxc--session-db-hash keepassxc--session))))

(ert-deftest keepassxc-tests-stored-association-reused ()
  "A pre-existing stored association is verified and reused."
  (keepassxc-tests--with-mock '(:associations (("stored-id" . "stored-key")))
    (setq keepassxc-tests--assocs
          '(("MOCKHASH" . (:id "stored-id" :id-key "stored-key"))))
    (should (= (length (keepassxc-get-logins "https://example.com")) 2))
    ;; The stored association was verified, none created.
    (should (= (length (keepassxc-mock-requests-for mock "inner:associate"))
               0))
    (should (>= (length (keepassxc-mock-requests-for
                         mock "inner:test-associate"))
                1))))


;;; Actions

(ert-deftest keepassxc-tests-generate-password ()
  "generate-password returns the password field of the reply."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-generate-password) "mock-generated-password"))))

(ert-deftest keepassxc-tests-get-totp ()
  "get-totp returns the TOTP for an entry uuid."
  (keepassxc-tests--with-mock nil
    (should (equal (keepassxc-get-totp "uuid-1") "123456"))))

(ert-deftest keepassxc-tests-create-and-delete-group-and-entry ()
  "Group creation, entry deletion and autotype round-trip."
  (keepassxc-tests--with-mock nil
    (should (equal (gethash "name" (keepassxc-create-new-group "emacs/mail"))
                   "emacs/mail"))
    (keepassxc-delete-entry "uuid-1")
    (should (equal (gethash "uuid"
                            (car (keepassxc-mock-requests-for
                                  mock "inner:delete-entry")))
                   "uuid-1"))
    (keepassxc-request-autotype "example.com")
    (should (equal (gethash "search"
                            (car (keepassxc-mock-requests-for
                                  mock "inner:request-autotype")))
                   "example.com"))))

(ert-deftest keepassxc-tests-lock-database ()
  "lock-database locks the mock database."
  (keepassxc-tests--with-mock nil
    (keepassxc-get-database-hash)
    (keepassxc-lock-database)
    (should (keepassxc-mock-locked mock))))


;;; Kill-ring handling

(ert-deftest keepassxc-tests-kill-secret-clears ()
  "Copied secrets are removed from the kill-ring after the timeout."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (keepassxc-password-timeout 0.1)
        (keepassxc--clear-timer nil)
        (keepassxc--pending-secret nil))
    (keepassxc--kill-secret "sekrit-pw" "Test secret")
    (should (equal (current-kill 0 t) "sekrit-pw"))
    (sit-for 0.3)
    (should-not (member "sekrit-pw" kill-ring))))

(ert-deftest keepassxc-tests-kill-secret-second-copy-clears-first ()
  "Copying a second secret clears the first one immediately."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (keepassxc-password-timeout 5)
        (keepassxc--clear-timer nil)
        (keepassxc--pending-secret nil))
    (keepassxc--kill-secret "first-pw" "First")
    (keepassxc--kill-secret "second-pw" "Second")
    (should-not (member "first-pw" kill-ring))
    (should (equal (current-kill 0 t) "second-pw"))
    (cancel-timer keepassxc--clear-timer)))

(ert-deftest keepassxc-tests-kill-secret-no-timeout ()
  "With `keepassxc-password-timeout' nil, secrets are kept."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (keepassxc-password-timeout nil)
        (keepassxc--clear-timer nil)
        (keepassxc--pending-secret nil))
    (keepassxc--kill-secret "sekrit-pw" "Test secret")
    (sit-for 0.2)
    (should (member "sekrit-pw" kill-ring))))


;;; auth-source backend

(ert-deftest keepassxc-tests-auth-source-search ()
  "Basic auth-source searches against the mock database."
  (keepassxc-tests--with-mock nil
    (let ((auth-sources '(keepassxc))
          (auth-source-do-cache nil))
      (let ((results (auth-source-search :host "example.com" :max 2)))
        (should (= (length results) 2))
        (let ((secret (plist-get (car results) :secret)))
          (should (functionp secret))
          (should (equal (funcall secret) "s3cret")))
        (should (equal (plist-get (car results) :user) "alice")))
      ;; :user filter
      (let ((results (auth-source-search :host "example.com"
                                         :user "bob" :max 5)))
        (should (= (length results) 1))
        (should (equal (funcall (plist-get (car results) :secret)) "hunter2")))
      ;; Port selects the URL scheme.
      (let ((results (auth-source-search :host "mail.example.com"
                                         :port 993 :max 1)))
        (should (= (length results) 1))
        (should (equal (funcall (plist-get (car results) :secret))
                       "imap-pass")))
      ;; :require drops incomplete results.
      (should (auth-source-search :host "example.com"
                                  :require '(:user :secret) :max 1))
      ;; :require is applied before :max, so an entry without a
      ;; username must not use up the budget.
      (let ((results (auth-source-search :host "empty.example.com"
                                         :require '(:user :secret) :max 1)))
        (should (= (length results) 1))
        (should (equal (plist-get (car results) :user) "eve")))
      ;; A list :port means "any of these".
      (let ((results (auth-source-search :host "mail.example.com"
                                         :port '("smtp" "imaps") :max 1)))
        (should (= (length results) 1))
        (should (equal (funcall (plist-get (car results) :secret))
                       "imap-pass")))
      ;; The wildcard :host t yields nil instead of crashing.
      (should-not (auth-source-search :host t))
      ;; Unknown hosts yield nil, not an error.
      (should-not (auth-source-search :host "unknown.example.com")))))

(ert-deftest keepassxc-tests-auth-source-locked-returns-nil ()
  "A locked database makes auth-source return nil instead of erroring."
  (keepassxc-tests--with-mock '(:locked t)
    (let ((auth-sources '(keepassxc))
          (auth-source-do-cache nil))
      (should-not (auth-source-search :host "example.com")))))

(ert-deftest keepassxc-tests-auth-source-create ()
  "auth-source :create builds an entry saved via set-login."
  (keepassxc-tests--with-mock nil
    (let* ((auth-sources '(keepassxc))
           (auth-source-do-cache nil)
           (results (cl-letf (((symbol-function 'read-passwd)
                               (lambda (&rest _) "new-pass")))
                      (auth-source-search :host "new.example.com"
                                          :user "carol" :create t)))
           (result (car results)))
      (should result)
      (should (equal (plist-get result :user) "carol"))
      (should (functionp (plist-get result :save-function)))
      (funcall (plist-get result :save-function))
      (let ((inner (car (keepassxc-mock-requests-for mock "inner:set-login"))))
        (should inner)
        (should (equal (gethash "url" inner) "https://new.example.com"))
        (should (equal (gethash "login" inner) "carol"))
        (should (equal (gethash "password" inner) "new-pass"))))))


;;; keepassxc-cli and application control

(ert-deftest keepassxc-tests-cli-output-and-quoting ()
  "`keepassxc--cli' shell-quotes args and returns trimmed stdout."
  (let ((keepassxc-cli-command "keepassxc-cli --quiet")
        captured)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (_input _end _program _delete _buffer _display
                        &rest args)
                 (setq captured (car (last args)))
                 (insert "s3cret!\n")
                 0)))
      (should (equal (keepassxc--cli nil "generate" "-x" "a b") "s3cret!"))
      (should (equal captured
                     (concat "keepassxc-cli --quiet generate -x "
                             (shell-quote-argument "a b")))))))

(ert-deftest keepassxc-tests-cli-nonzero-exit ()
  "`keepassxc--cli' signals a `user-error' on non-zero exit."
  (cl-letf (((symbol-function 'call-process-region)
             (lambda (&rest _) 1)))
    (should-error (keepassxc--cli nil "generate") :type 'user-error)))

(ert-deftest keepassxc-tests-cli-input-via-stdin ()
  "`keepassxc--cli' passes INPUT on stdin, never in the command line."
  (let (captured-input captured-command)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (input _end _program _delete _buffer _display
                        &rest args)
                 (setq captured-input input
                       captured-command (car (last args)))
                 (insert "Entropy: 42\n")
                 0)))
      (should (equal (keepassxc-cli-estimate-password "hunter2")
                     "Entropy: 42"))
      (should (equal captured-input "hunter2\n"))
      (should-not (string-search "hunter2" captured-command)))))

(ert-deftest keepassxc-tests-cli-generate-args ()
  "`keepassxc-cli-generate-password' passes length and options."
  (let ((keepassxc-cli-generate-options '("--upper" "--numeric"))
        captured)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (_input _end _program _delete _buffer _display
                        &rest args)
                 (setq captured (car (last args)))
                 (insert "pw\n")
                 0)))
      (should (equal (keepassxc-cli-generate-password 20) "pw"))
      (should (string-search " generate -L 20 --upper --numeric" captured)))))

(ert-deftest keepassxc-tests-lock-all-databases-fallback ()
  "Without D-Bus, locking shells out to `keepassxc-command' --lock."
  (let ((keepassxc-command "flatpak run org.keepassxc.KeePassXC")
        captured)
    (cl-letf (((symbol-function 'keepassxc--dbus-available-p)
               (lambda () nil))
              ((symbol-function 'call-process-shell-command)
               (lambda (command &rest _)
                 (setq captured command)
                 0)))
      (keepassxc-lock-all-databases)
      (should (equal captured
                     "flatpak run org.keepassxc.KeePassXC --lock")))
    (cl-letf (((symbol-function 'keepassxc--dbus-available-p)
               (lambda () nil))
              ((symbol-function 'call-process-shell-command)
               (lambda (&rest _) 1)))
      (should-error (keepassxc-lock-all-databases) :type 'user-error))))

(ert-deftest keepassxc-tests-lock-all-databases-dbus-error-falls-back ()
  "A failing D-Bus call falls back to the command line."
  (unless (get 'dbus-error 'error-conditions)
    (define-error 'dbus-error "D-Bus error"))
  (let ((keepassxc-command "keepassxc")
        captured)
    (cl-letf (((symbol-function 'keepassxc--dbus-available-p)
               (lambda () t))
              ((symbol-function 'keepassxc--call-dbus-method)
               (lambda (&rest _) (signal 'dbus-error '("no such service"))))
              ((symbol-function 'call-process-shell-command)
               (lambda (command &rest _)
                 (setq captured command)
                 0)))
      (keepassxc-lock-all-databases)
      (should (equal captured "keepassxc --lock")))))

(ert-deftest keepassxc-tests-open-database-fallback ()
  "Without D-Bus, opening a database shells out to `keepassxc-command'."
  (let ((keepassxc-command "keepassxc")
        captured)
    (cl-letf (((symbol-function 'keepassxc--dbus-available-p)
               (lambda () nil))
              ((symbol-function 'start-process-shell-command)
               (lambda (_name _buffer command)
                 (setq captured command)
                 nil))
              ((symbol-function 'set-process-sentinel) #'ignore))
      (keepassxc-open-database "~/passwords/my db.kdbx")
      (should (equal captured
                     (concat "keepassxc "
                             (shell-quote-argument
                              (expand-file-name
                               "~/passwords/my db.kdbx"))))))))


;;; Plstore persistence (needs gpg)

(ert-deftest keepassxc-tests-plstore-roundtrip ()
  "Associations survive a plstore round-trip, id-key encrypted at rest."
  (skip-unless (executable-find "gpg"))
  (let* ((dir (make-temp-file "keepassxc-gpg" t))
         (gnupghome (expand-file-name "gnupg" dir)))
    (make-directory gnupghome)
    (set-file-modes gnupghome #o700)
    (unwind-protect
        (with-environment-variables (("GNUPGHOME" gnupghome))
          (skip-unless
           (eq 0 (call-process "gpg" nil nil nil
                               "--batch" "--pinentry-mode" "loopback"
                               "--passphrase" "" "--quick-gen-key"
                               "keepassxc-test@example.invalid"
                               "default" "default" "never")))
          ;; The subdirectory does not exist yet; it must be created.
          (let ((keepassxc-association-file
                 (expand-file-name "sub/dir/assoc.plist" dir))
                (keepassxc--plstore nil)
                (keepassxc--plstore-file nil)
                (plstore-encrypt-to '("keepassxc-test@example.invalid"))
                (epg-pinentry-mode 'loopback))
            (keepassxc--association-put "HASH1" "id-1" "key-1")
            ;; Force re-reading and decrypting from disk.
            (setq keepassxc--plstore nil
                  keepassxc--plstore-file nil)
            (let ((association (keepassxc--association-get "HASH1")))
              (should (equal (plist-get association :id) "id-1"))
              (should (equal (plist-get association :id-key) "key-1")))
            ;; The id-key must not be stored in cleartext.
            (with-temp-buffer
              (insert-file-contents keepassxc-association-file)
              (goto-char (point-min))
              (should-not (search-forward "key-1" nil t))
              (goto-char (point-min))
              (should (search-forward "id-1" nil t)))))
      (delete-directory dir t))))

(provide 'keepassxc-tests)
;;; keepassxc-tests.el ends here
