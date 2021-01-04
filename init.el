(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq cursor-type 'bar)
(setq inhibit-splash-screen 1)

(set-face-attribute 'default nil :height 140)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

（global-company-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
