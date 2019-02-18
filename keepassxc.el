;;; keepassxc.el --- Utility functions for working with keepassxc -*- lexical-binding: t -*-

;; Copyright (c) 2019 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/keepassxc.el
;; Keywords: keepassxc, keepass, convenience, tools
;; Version: 0.1
;; Package-Requires: ((emacs "25.2"))

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

(require 'dbus)


;;; Customization

(defgroup keepassxc nil
  "KeePassXC integration"
  :repfix "keepassxc-"
  :group 'tools)

(defcustom keepassxc-database-file nil
  "Database file that keepass commands use by default."
  :type 'file)

(defcustom keepassxc-key-file nil
  "Key file that keepass commands use by default."
  :type 'file)


;;; DBus interface

;; https://github.com/keepassxreboot/keepassxc/wiki/Using-DBus-with-KeePassXC
(defun keepassxc--call-dbus-method (method &rest args)
  "Call keepass METHOD on dbus with optional ARGS."
  (apply #'dbus-call-method :session
         "org.keepassxc.KeePassXC.MainWindow"
         "/keepassxc" "org.keepassxc.MainWindow" method args))

(defun keepassxc-lock-all-databases ()
  "Lock all databases."
  (interactive)
  (keepassxc--call-dbus-method "lockAllDatabases"))

(defun keepassxc-close-all-databases ()
  "Close all databases."
  (interactive)
  (keepassxc--call-dbus-method "closeAllDatabases"))

(defun keepassxc-exit ()
  "Exit KeePassXC app."
  (interactive)
  (keepassxc--call-dbus-method "appExit"))

(defun keepassxc-open-database (database)
  "Open DATABASE file in KeePassXC."
  (interactive
   (list (or keepassxc-database-file (read-file-name "Database file: "))))
  (keepassxc--call-dbus-method  "openDatabase" database))

(defun keepassxc-open-database-password (database password)
  "Open DATABASE file with PASSWORD in KeePassXC."
  (interactive
   (let* ((db (or keepassxc-database-file (read-file-name "Database file: ")))
          (pass (read-passwd (format "Password for %s: " (file-name-nondirectory db)))))
     (list db pass)))
  (keepassxc--call-dbus-method  "openDatabase" database password))

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
;; FIXME: Need to create libsodium bindings first

(defvar keepassxc--socket-name "kpxc_server"
  "Filename of the KeePassXC unix domain socket.")

(defun keepassxc--get-process ()
  "Return keepassxc process."
  (or (get-process "keepassxc")
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

(defun keepassxc--make-process ()
  "Make a network process to KeePassXC."
  (make-network-process
   :name "keepassxc"
   :family 'local
   :remote (keepassxc--get-socket-file)
   :buffer "*keepassxc*"))

(defun keepassxc--send-msg (msg)
  "Send MSG to KeePassXC."
  (process-send-string
   (keepassxc--get-process)
   (json-serialize msg)))


(provide 'keepassxc)
;;; keepassxc.el ends here
