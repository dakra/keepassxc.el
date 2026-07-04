;;; package-lint-batch.el --- Run package-lint from MELPA -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: emacs -Q --batch -L . -l scripts/package-lint-batch.el FILE...
;; Installs package-lint from MELPA into a scratch directory and lints
;; the given files.  Errors about the `sodium' dependency not being
;; installable are ignored: sodium.el is a dynamic module distributed
;; outside of ELPA/MELPA.

;;; Code:

(require 'package)
(require 'cl-lib)

(setq package-user-dir (expand-file-name ".package-lint-elpa"
                                         default-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'package-lint)
  (package-refresh-contents)
  (package-install 'package-lint))
(require 'package-lint)

(setq package-lint-main-file "keepassxc.el")

(let ((issues 0))
  (dolist (file command-line-args-left)
    (with-temp-buffer
      (insert-file-contents (expand-file-name file) t)
      (emacs-lisp-mode)
      (dolist (result (package-lint-buffer))
        (cl-destructuring-bind (line col type message) result
          (unless (string-match-p "installable" message)
            (setq issues (1+ issues))
            (message "%s:%d:%d: %s: %s" file line col type message))))))
  (kill-emacs (if (> issues 0) 1 0)))

;;; package-lint-batch.el ends here
