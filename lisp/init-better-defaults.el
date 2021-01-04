(setq ring-bell-function 'ignore)

(global-auto-revert-mode 1)

(global-linum-mode 1)

(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("8zl" "liwei")
					    ;; Apple
					    ("8ms" "Apple")
					    ))

(setq make-backup-files nil)
(setq auto-save-default nil)

(recentf-mode 1)
(setq recentf-max-menu-item 25)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(delete-selection-mode 1)

(provide 'init-better-defaults)
