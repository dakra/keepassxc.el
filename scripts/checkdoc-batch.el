;;; checkdoc-batch.el --- Run checkdoc, fail on diagnostics -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: emacs -Q --batch -l scripts/checkdoc-batch.el FILE...
;; Exits non-zero when checkdoc reports any issue.

;;; Code:

(require 'checkdoc)

(let ((issues 0))
  (advice-add 'checkdoc-error :around
              (lambda (orig &rest args)
                (setq issues (1+ issues))
                (apply orig args)))
  (dolist (file command-line-args-left)
    (checkdoc-file (expand-file-name file)))
  (when (> issues 0)
    (message "checkdoc: %d issue(s) found" issues)
    (kill-emacs 1)))

;;; checkdoc-batch.el ends here
