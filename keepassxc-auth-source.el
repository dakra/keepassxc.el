;;; keepassxc-auth-source.el --- KeePassXC backend for auth-source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/keepassxc.el

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

;; An `auth-source' backend that looks up (and creates) credentials in
;; KeePassXC via the browser-integration protocol from keepassxc.el.
;;
;; Enable it with:
;;
;;   (keepassxc-auth-source-enable)
;;
;; Afterwards `auth-source-search' (and thus Gnus, ERC, smtpmail,
;; Forge and friends) will query the KeePassXC database:
;;
;;   (auth-source-search :host "irc.libera.chat" :port "6697")
;;
;; Hosts are looked up as URLs; the port (number or service name)
;; selects the URL scheme, see `keepassxc-auth-source-port-scheme-alist'.
;; Without a scheme-mapped port, entries use the dedicated scheme
;; `keepassxc-auth-source-scheme' (auth-source://host, or
;; auth-source://host:port for unmapped numeric ports), which the
;; KeePassXC browser extension does not offer on websites.
;; With :create t a missing entry is created in KeePassXC when the
;; caller invokes the returned :save-function.
;; Set `keepassxc-auth-source-group' to save such entries into a specific group.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'keepassxc)

(defcustom keepassxc-auth-source-port-scheme-alist
  '((21    . "ftp")
    (22    . "ssh")
    (23    . "telnet")
    (25    . "smtp")
    (80    . "http")
    (110   . "pop3")
    (119   . "nntp")
    (143   . "imap")
    (194   . "irc")
    (389   . "ldap")
    (443   . "https")
    (445   . "smb")
    (465   . "smtps")
    (563   . "nntps")
    (587   . "smtp")
    (636   . "ldaps")
    (990   . "ftps")
    (993   . "imaps")
    (995   . "pop3s")
    (1433  . "mssql")
    (1521  . "oracle")
    (1883  . "mqtt")
    (3306  . "mysql")
    (3389  . "rdp")
    (5222  . "xmpp")
    (5432  . "postgresql")
    (5672  . "amqp")
    (5900  . "vnc")
    (6379  . "redis")
    (6667  . "irc")
    (6697  . "ircs")
    (27017 . "mongodb"))
  "Alist mapping numeric ports to URL schemes for KeePassXC lookups."
  :type '(alist :key-type integer :value-type string)
  :group 'keepassxc)

(defcustom keepassxc-auth-source-group "Emacs/auth-source"
  "KeePassXC group for entries created via auth-source.
A slash-separated path like \"emacs/mail\" (nested groups), as in
the browser extension's group setting.  A missing group is
created after a KeePassXC confirmation dialog.
When nil, KeePassXC puts new entries into its default browser group."
  :type '(choice (const :tag "KeePassXC default group" nil)
                 (string :tag "Group path"))
  :group 'keepassxc)

(defcustom keepassxc-auth-source-scheme "auth-source"
  "URL scheme for KeePassXC entries only the auth-source backend matches.
Entries imported or created without a port-derived URL scheme use
this scheme, e.g. \"auth-source://api.openai.com\".  The KeePassXC
browser extension does not offer such entries on websites (with
its default \"Match URL scheme\" setting); lookups through this
backend always try this scheme as fallback."
  :type 'string
  :group 'keepassxc)

(defun keepassxc-auth-source--urls (host port)
  "Return candidate KeePassXC URLs for HOST and PORT, best match first.
A non-numeric PORT (service name like \"imaps\") is used as URL
scheme directly; a numeric PORT is translated with
`keepassxc-auth-source-port-scheme-alist'.  A numeric PORT
without scheme mapping keeps the port, as
`keepassxc-auth-source-scheme'://HOST:PORT.  HOST with
`keepassxc-auth-source-scheme' and with
`keepassxc-default-url-schema' are always included as fallbacks.
A HOST that already contains a URL scheme is used as-is."
  (let* ((port (cond ((null port) nil)
                     ((symbolp port) (symbol-name port))
                     ((numberp port) (number-to-string port))
                     (t port)))
         (numeric (and port (string-match-p "\\`[0-9]+\\'" port)))
         (scheme (cond ((null port) nil)
                       ((not numeric) port)
                       (t (alist-get (string-to-number port)
                                     keepassxc-auth-source-port-scheme-alist)))))
    (if (string-match-p "://" host)
        (list host)
      (delete-dups
       (delq nil
             (list (when scheme (format "%s://%s" scheme host))
                   (when (and numeric (not scheme))
                     (format "%s://%s:%s"
                             keepassxc-auth-source-scheme host port))
                   (format "%s://%s" keepassxc-auth-source-scheme host)
                   (concat keepassxc-default-url-schema host)))))))

(defun keepassxc-auth-source--create (spec)
  "Return a list with one new auth-source entry for SPEC.
The entry is only stored in KeePassXC when the caller funcalls
its :save-function, as per the auth-source create protocol."
  (let* ((host (seq-find #'stringp (ensure-list (plist-get spec :host))))
         (port (seq-find (lambda (p) (not (eq p t)))
                         (ensure-list (plist-get spec :port))))
         (url (car (keepassxc-auth-source--urls host port)))
         (user (or (car (ensure-list (plist-get spec :user)))
                   (read-string (format "KeePassXC username for %s: " url))))
         (secret (or (plist-get spec :secret)
                     (read-passwd (format "KeePassXC password for %s@%s: "
                                          user url)
                                  t))))
    (list
     `(:host ,host
       ,@(when port (list :port port))
       :user ,user
       :secret ,(lambda () secret)
       :save-function ,(lambda ()
                         (if-let* ((group keepassxc-auth-source-group)
                                   (uuid (gethash
                                          "uuid"
                                          (keepassxc-create-new-group group))))
                             (keepassxc-set-login url user secret
                                                  nil group uuid)
                           (keepassxc-set-login url user secret))
                         (message "Saved %s@%s in KeePassXC" user url))))))

;;;###autoload
(defun keepassxc-auth-source-import ()
  "Import the entries of all other auth-sources into KeePassXC.
Collect every entry with a host and a secret from the backends in
`auth-sources' except the KeePassXC backend itself (e.g. ~/.authinfo.gpg)
and save it in KeePassXC, in `keepassxc-auth-source-group'.
Hosts are turned into URLs as in lookups, see `keepassxc-auth-source--urls'.
Entries whose URL already has a login with the same username in KeePassXC are
skipped, so the import can be re-run safely."
  (interactive)
  (let* ((auth-sources (remq 'keepassxc auth-sources))
         (auth-source-do-cache nil)
         (entries (seq-filter (lambda (entry)
                                (and (stringp (plist-get entry :host))
                                     (plist-get entry :secret)))
                              (auth-source-search :max most-positive-fixnum)))
         (imported 0)
         (skipped 0)
         (seen nil))
    (unless entries
      (user-error "No auth-source entries to import"))
    (unless (y-or-n-p (format "Import %d auth-source entries into KeePassXC? "
                              (length entries)))
      (user-error "Import aborted"))
    (let* ((group keepassxc-auth-source-group)
           (group-uuid (when group
                         (gethash "uuid" (keepassxc-create-new-group group))))
           (reporter (make-progress-reporter "Importing into KeePassXC..."
                                             0 (length entries)))
           (i 0))
      (dolist (entry entries)
        (let* ((user (or (plist-get entry :user) ""))
               (secret (plist-get entry :secret))
               (password (if (functionp secret) (funcall secret) secret))
               (url (car (keepassxc-auth-source--urls
                          (plist-get entry :host) (plist-get entry :port)))))
          (if (or (not (stringp password))
                  (member (cons url user) seen)
                  (seq-find (lambda (login)
                              (equal (gethash "login" login) user))
                            (condition-case nil
                                (keepassxc-get-logins url)
                              (keepassxc-no-logins nil))))
              (cl-incf skipped)
            (keepassxc-set-login url user password nil group group-uuid)
            (cl-incf imported))
          (push (cons url user) seen))
        (progress-reporter-update reporter (cl-incf i)))
      (progress-reporter-done reporter))
    (message "Imported %d auth-source entries into KeePassXC (%d skipped)"
             imported skipped)))

(cl-defun keepassxc-auth-source-search (&rest spec
                                        &key backend type host user port
                                        max require create delete
                                        &allow-other-keys)
  "Search KeePassXC for entries matching SPEC.
See `auth-source-search' for the meaning of HOST, USER, PORT,
MAX, REQUIRE, CREATE and DELETE.  TYPE and BACKEND identify this
backend.  Returns a list of plists with :host, :port, :user and
a :secret function.  Connection problems, a locked database or a
declined association yield nil instead of an error."
  (cl-assert (or (null type) (eq type (slot-value backend 'type))) t)
  ;; Wildcards (t) cannot be enumerated over the browser protocol.
  (let ((hosts (seq-filter #'stringp (ensure-list host))))
    (cond
     (delete
      (warn "The keepassxc auth-source backend does not support deletion")
      nil)
     ((null hosts) nil)
     (t
      (condition-case err
          (let ((ports (or (seq-remove (lambda (p) (eq p t))
                                       (ensure-list port))
                           '(nil)))
                (users (and user (ensure-list user)))
                (max (or max 1))
                (tried-urls nil)
                (seen-uuids nil)
                (results nil))
            (catch 'keepassxc--max
              (dolist (h hosts)
                (dolist (p ports)
                  (dolist (url (keepassxc-auth-source--urls h p))
                    (unless (member url tried-urls)
                      (push url tried-urls)
                      (dolist (entry (condition-case nil
                                         (keepassxc-get-logins url)
                                       (keepassxc-no-logins nil)))
                        (let* ((uuid (gethash "uuid" entry))
                               (login (gethash "login" entry))
                               (password (gethash "password" entry))
                               (result
                                `(:host ,h
                                  ,@(when p (list :port p))
                                  ,@(when (and login
                                               (not (string-empty-p login)))
                                      (list :user login))
                                  :secret ,(lambda () password))))
                          (when (and (not (member uuid seen-uuids))
                                     (or (null users) (member login users))
                                     (seq-every-p (lambda (key)
                                                    (plist-get result key))
                                                  require))
                            (push uuid seen-uuids)
                            (push result results)
                            (when (>= (length results) max)
                              (throw 'keepassxc--max nil))))))))))
            (or (nreverse results)
                (and create (keepassxc-auth-source--create spec))))
        (keepassxc-error
         (message "keepassxc-auth-source: %s" (error-message-string err))
         nil))))))

(defvar keepassxc-auth-source-backend
  (auth-source-backend :source "." ;; not used
                       :type 'keepassxc
                       :search-function #'keepassxc-auth-source-search)
  "Auth-source backend for KeePassXC.")

(defun keepassxc-auth-source-backend-parse (entry)
  "Create a KeePassXC auth-source backend from ENTRY."
  (when (eq entry 'keepassxc)
    (auth-source-backend-parse-parameters entry keepassxc-auth-source-backend)))

(add-hook 'auth-source-backend-parser-functions
          #'keepassxc-auth-source-backend-parse)

(defun keepassxc-auth-source-clear-cache ()
  "Forget all cached auth-source results."
  (interactive)
  (auth-source-forget-all-cached))

(defun keepassxc-auth-source--on-signal (action _msg)
  "Flush auth-source caches on KeePassXC database lock and unlock.
ACTION is the signal name sent by KeePassXC.  In particular,
negative results cached while the database was locked are
forgotten as soon as it is unlocked."
  (when (member action '("database-locked" "database-unlocked"))
    (auth-source-forget-all-cached)))

;;;###autoload
(defun keepassxc-auth-source-enable ()
  "Enable the KeePassXC auth-source backend.
Add the symbol `keepassxc' to `auth-sources' and flush cached
auth-source results."
  (interactive)
  (add-to-list 'auth-sources 'keepassxc)
  (add-hook 'keepassxc-signal-functions #'keepassxc-auth-source--on-signal)
  (auth-source-forget-all-cached))

(provide 'keepassxc-auth-source)
;;; keepassxc-auth-source.el ends here
