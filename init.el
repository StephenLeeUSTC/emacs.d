(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
 (require 'cl)

 ;; Add Packages
 (defvar my/packages '(
                ;; --- Auto-completion ---
                company
                ;; --- Better Editor ---
                hungry-delete
                swiper
                counsel
                smartparens
                ;; --- Major Mode ---
                js2-mode
                ;; --- Minor Mode ---
                nodejs-repl
                exec-path-from-shell
                ;; --- Themes ---
                monokai-theme
                ;; solarized-theme
                ) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
           when (not (package-installed-p pkg)) do (return nil)
           finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
         (package-install pkg))))

 ;; Find Executable Path on OS X
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(global-hl-line-mode 1)

(load-theme 'monokai 1)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq-default cursor-type 'bar)
(setq inhibit-splash-screen 1)

(set-face-attribute 'default nil :height 140)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-company-mode 1)

(setq make-backup-files nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; this can be replaced by counsel

;; js
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

;; cc, c
(setq auto-mode-alist
  (append
   ;; File name (within directory) starts with a dot.
   '(("/\\.[^/]*\\'" . fundamental-mode)
     ;; File name has no dot.
     ("/[^\\./]*\\'" . fundamental-mode)
     ;; File name ends in ‘.C’.
     ("\\.C\\'" . c++-mode))
   auto-mode-alist))

(require 'org)
(setq org-src-fontify-natively t)

(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)

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
