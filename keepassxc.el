;;; keepassxc.el --- Utility functions for working with keepassxc -*- lexical-binding: t -*-

;; Copyright (c) 2019-2021 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/keepassxc.el
;; Keywords: keepassxc, keepass, convenience, tools
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (sodium "0.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `keepassxc.el' provides utilities for working with keepassxc

;;; Code:

(require 'browse-url)
(require 'dbus)
(require 'sodium)



;;; Customization

(defgroup keepassxc nil
  "KeePassXC integration"
  :prefix "keepassxc-"
  :group 'tools
  :link '(url-link "https://github.com/dakra/keepassxc.el"))

(defcustom keepassxc-command "keepassxc"
  "Filename of the keepassxc executable."
  :type 'file)

(defcustom keepassxc-database-file nil
  "Database file that keepass commands use by default."
  :type 'file)

(defcustom keepassxc-key-file nil
  "Key file that keepass commands use by default."
  :type 'file)

(defcustom keepassxc-client-id "emacs-keepassxc"
  "Identity of the Emacs client."
  :type 'string)

(defcustom keepassxc-save-file
  (expand-file-name "var/keepassxc.el" user-emacs-directory)
  "File in which to save your keyring."
  :type 'file)

(defcustom keepassxc-default-url-schema "https://"
  "Default schema to use when searching (e.g. with `get-login') for a URL and no schema is provided."
  :type 'string)


;;; DBus interface

;; https://github.com/keepassxreboot/keepassxc/wiki/Using-DBus-with-KeePassXC
(defun keepassxc--call-dbus-method (method &rest args)
  "Call keepass METHOD on dbus with optional ARGS."
  (apply #'dbus-call-method :session
         "org.keepassxc.KeePassXC.MainWindow"
         "/keepassxc" "org.keepassxc.MainWindow" method args))

;;;###autoload
(defun keepassxc-open ()
  "Start keepassxc."
  (interactive)
  (start-process-shell-command "keepassxc" nil keepassxc-command))

;;;###autoload
(defun keepassxc-lock-all-databases ()
  "Lock all databases."
  (interactive)
  (keepassxc--call-dbus-method "lockAllDatabases"))

;;;###autoload
(defun keepassxc-close-all-databases ()
  "Close all databases."
  (interactive)
  (keepassxc--call-dbus-method "closeAllDatabases"))

;;;###autoload
(defun keepassxc-exit ()
  "Exit KeePassXC app."
  (interactive)
  (keepassxc--call-dbus-method "appExit"))

;;;###autoload
(defun keepassxc-open-database (database)
  "Open DATABASE file in KeePassXC."
  (interactive
   (list (or keepassxc-database-file (read-file-name "Database file: "))))
  (keepassxc--call-dbus-method  "openDatabase" database))

;;;###autoload
(defun keepassxc-open-database-password (database password)
  "Open DATABASE file with PASSWORD in KeePassXC."
  (interactive
   (let* ((db (or keepassxc-database-file (read-file-name "Database file: ")))
          (pass (read-passwd (format "Password for %s: " (file-name-nondirectory db)))))
     (list db pass)))
  (keepassxc--call-dbus-method  "openDatabase" database password))

;;;###autoload
(defun keepassxc-open-database-password-key (database password keyfile)
  "Open DATABASE file with PASSWORD and KEYFILE in KeePassXC."
  (interactive
   (let* ((db (or keepassxc-database-file (read-file-name "Database file: ")))
          (pass (read-passwd (format "Password for %s: " (file-name-nondirectory db))))
          (kf (or keepassxc-key-file
                  (read-file-name (format "Key file for %s: " (file-name-nondirectory db))))))
     (list db pass kf)))
  (keepassxc--call-dbus-method  "openDatabase" database password keyfile))


;;; Socket interface

(defvar keepassxc--socket-name "org.keepassxc.KeePassXC.BrowserServer"
  "Filename of the KeePassXC unix domain socket.")
(defvar keepassxc--process-name " *keepassxc-socket-process*")
(defvar keepassxc--keypair nil)
(defvar keepassxc--server-key nil)
(defvar keepassxc--id nil)
(defvar keepassxc--id-key nil)
(defvar keepassxc--next-nonce nil)
(defvar keepassxc--last-msg nil)
(defvar keepassxc--last-filter-input nil)


(defun keepassxc--read-url ()
  "Read URL from the user return it with default schema prefixed if non given."
  (let ((url (car (browse-url-interactive-arg "URL: "))))
    (if (string-match-p "://" url)
        url
      (concat keepassxc-default-url-schema url))))

(defun keepassxc--save-keys ()
  "Save keyring to `keepassxc-save-file'."
  (make-directory (file-name-directory (expand-file-name keepassxc-save-file)) 'parents)
  (with-temp-file keepassxc-save-file
    (insert (format "(setq keepassxc--keypair '((pk . \"%s\") (sk . \"%s\")))\n"
                    (alist-get 'pk keepassxc--keypair) (alist-get 'sk keepassxc--keypair)))
    (insert (format "(setq keepassxc--id \"%s\")\n" keepassxc--id))
    (insert (format "(setq keepassxc--id-key \"%s\")\n" keepassxc--id-key))))

(defun keepassxc--load-keys ()
  "Load keys from `keepassxc-save-file'."
  (when (file-exists-p keepassxc-save-file)
    (load keepassxc-save-file t)
    (unless (keepassxc-test-associate)
      (user-error "Keys loaded but not associated"))
    t))

(defun keepassxc--get-process ()
  "Return keepassxc process."
  (or (get-process keepassxc--process-name)
      (keepassxc--make-process)))

(defun keepassxc--get-socket-file ()
  "Return filename of the KeePassXC socket file or NIL if not found."
  (let ((xdg-fn (expand-file-name keepassxc--socket-name (getenv "XDG_RUNTIME_DIR")))
        (tmp-fn (expand-file-name keepassxc--socket-name (temporary-file-directory))))
    (if (file-exists-p xdg-fn)
        xdg-fn
      (if (file-exists-p tmp-fn)
          tmp-fn
        (error "Can't locate KeePassXC socket")))))

(defun keepassxc--filter (_p msg)
  "Filter function to check messages MSG from the keepassxc process."
  ;; Check if there was an incomplete JSON string at last filter function invocation.
  (when keepassxc--last-filter-input
    (setq msg (concat keepassxc--last-filter-input msg))
    (setq keepassxc--last-filter-input nil))
  (condition-case err
      (let* ((m (json-parse-string msg))
             (nonce (gethash "nonce" m))
             (cipher (gethash "message" m))
             (pk keepassxc--server-key)
             (sk (alist-get 'sk keepassxc--keypair)))
        (when (and nonce (not (string-equal nonce keepassxc--next-nonce)))
          (error "Nonce check failed: %s != %s" nonce keepassxc--next-nonce))

        (setq keepassxc--last-msg
              (if (and nonce cipher)
                  (json-parse-string
                   (sodium-box-open cipher nonce pk sk))
                m)))
    ;; FIXME: Sometimes the keepass process sends rubbish after the json object
    ;;        Mostly only one character like . Find out why this is and maybe fix upstream?!
    ;; (json-trailing-content (keepassxc--filter nil (replace-regexp-in-string "[^}]+\\'" "" msg)))
    (json-trailing-content (error "%s - in json: %s" (error-message-string err) msg))
    ;; Filter function received msg before the JSON string got completely send
    (json-end-of-file (setq keepassxc--last-filter-input msg))))

(defun keepassxc--make-process ()
  "Make a network process to KeePassXC."
  (make-network-process
   :name keepassxc--process-name
   :family 'local
   :remote (keepassxc--get-socket-file)
   :filter #'keepassxc--filter
   :noquery t))

(defun keepassxc--get-nonce ()
  "Return a new nonce and set `keepassxc--next-nonce' to the increment of it."
  (let ((nonce (sodium-box-make-nonce)))
    (setq keepassxc--next-nonce (sodium-increment nonce))
    nonce))

(defun keepassxc--send-json (msg &optional timeout)
  "JSON serialize MSG and send to KeePassXC.
Wait for reply TIMEOUT seconds."
  (setq keepassxc--last-msg nil)
  (let ((p (keepassxc--get-process))
        (json-msg (json-serialize msg)))
    (process-send-string p json-msg)
    (unless (accept-process-output p (or timeout 2) nil t)
      (error "Timeout - No response from KeePassXC"))))

(defun keepassxc--send-action (action &optional msg timeout)
  "Send ACTION with MSG to keepassxc and wait for reply TIMEOUT seconds."
  ;; Error if we don't have a keypair or id yet
  (unless (or (string-equal action "associate")
              (and keepassxc--keypair keepassxc--id)
              (keepassxc--load-keys))
    (user-error "No keypair saved yet.  Run `keepassxc-associate'"))
  ;; If we don't have a server key yet, fetch one
  (unless keepassxc--server-key
    (keepassxc--get-server-key))

  (let* ((pk       keepassxc--server-key)
         (sk       (alist-get 'sk keepassxc--keypair))
         (json-msg (json-serialize (plist-put msg :action action)))
         (nonce    (keepassxc--get-nonce))
         (enc-msg  (sodium-box json-msg nonce pk sk)))
    (keepassxc--send-json `(:triggerUnlock t
                            :action ,action
                            :message ,enc-msg
                            :nonce ,nonce
                            :clientID ,keepassxc-client-id)
                          timeout)))

(defun keepassxc--get-server-key ()
  "Retrieve public key from KeePassXC used for this session."
  (keepassxc--send-json `(:triggerUnlock t
                          :action "change-public-keys"
                          :publicKey ,(cdr (assoc 'pk keepassxc--keypair))
                          :nonce ,(keepassxc--get-nonce)
                          :clientID ,keepassxc-client-id))
  (setq keepassxc--server-key (gethash "publicKey" keepassxc--last-msg)))


;;; Interactive commands

;;;###autoload
(defun keepassxc-associate ()
  "Connect to keepassxc socket and return database hash."
  (interactive)
  ;; Create a new keypair and new id key
  (setq keepassxc--keypair (sodium-box-keypair))
  (setq keepassxc--id-key (alist-get 'pk (sodium-box-keypair)))
  (setq keepassxc--id nil)
  (setq keepassxc--server-key nil)

  (keepassxc--send-action
   "associate"
   `(:key ,(alist-get 'pk keepassxc--keypair)
     :idKey ,keepassxc--id-key) 30)  ; Give the user 30 seconds to enter ID
  (when (gethash "id" keepassxc--last-msg)
    (setq keepassxc--id (gethash "id" keepassxc--last-msg))
    (keepassxc--save-keys)))

(defun keepassxc-test-associate ()
  "Test if this session is associated with KeePassXC.
Return the associated id or NIL."
  (keepassxc--send-action
   "test-associate"
   `(:id ,keepassxc--id :key ,keepassxc--id-key))
  (gethash "id" keepassxc--last-msg))

(defun keepassxc-get-database-hash ()
  "Return database hash."
  (keepassxc--send-action "get-databasehash")
  (gethash "hash" keepassxc--last-msg))

;;;###autoload
(defun keepassxc-get-logins (url)
  "Return logins for URL."
  (interactive (list (keepassxc--read-url)))
  (keepassxc--send-action
   "get-logins"
   `(:id ,keepassxc--id
     :url ,url
     :submitUrl ,url
     :keys [(:id ,keepassxc--id :key ,keepassxc--id-key)]))
  (gethash "entries" keepassxc--last-msg))

;;;###autoload
(defun keepassxc-get-login (url)
  "Return login for URL.

Lets you choose login with multiple matches."
  (interactive (list (keepassxc--read-url)))
  (let* ((logins (mapcar (lambda (e)
                           (cons (format "%s - %s" (gethash "name" e) (gethash "login" e)) e))
                         (keepassxc-get-logins url)))
         (login (cdr (assoc-string (completing-read "Select login: " logins) logins))))
    ;; (kill-new (gethash "name" e))
    ;; (kill-new (gethash "uuid" e))
    (kill-new url)
    (kill-new (gethash "login" login))
    (kill-new (gethash "password" login))
    (message "URL, login, password for %s - %s copied as last 3 items to the kill-ring."
             url (gethash "login" login))))

;;;###autoload
(defun keepassxc-set-login (uuid login password url)
  "Set login for UUID with LOGIN, PASSWORD and URL."
  (keepassxc--send-action
   "set-login"
   `(:id ,keepassxc--id-key
     :uuid ,uuid
     :login ,login
     :password ,password
     :url ,url)))

;;;###autoload
(defun keepassxc-generate-password ()
  "Generate a password using KeePassXC settings."
  (interactive)
  (keepassxc--send-action "generate-password")
  (kill-new (gethash "password" (aref (gethash "entries" keepassxc--last-msg) 0)))
  (message "Password copied to kill-ring."))

;;;###autoload
(defun keepassxc-lock-database ()
  "Lock the current database."
  (keepassxc--send-action "lock-database"))

(defun keepassxc-disconnect ()
  "Disconnect from KeePassXC socket."
  (setq keepassxc--server-key nil)
  (process-send-eof (keepassxc--get-process)))

(provide 'keepassxc)
;;; keepassxc.el ends here
