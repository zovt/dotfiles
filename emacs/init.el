;; basic visual
(set-face-font 'default "Iosevka-15")
(global-prettify-symbols-mode)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq-default truncate-lines t)

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; backups
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

;; packaging
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn (package-refresh-contents) (package-install 'use-package)))

;; start server
(add-hook 'after-init-hook 'server-start)

;; modes and plugins

;; electric pair mode
(electric-pair-mode)

;; undo tree
(use-package undo-tree :ensure t)

;; company
(use-package company :ensure t
  :config
  (setq-default company-idle-delay 0.3)
  (setq-default company-minimum-prefix-length 4)
  (add-hook 'after-init-hook 'global-company-mode))

;; flycheck
(use-package flycheck :ensure t :init (global-flycheck-mode))

;; smex
(use-package smex :ensure t :config (global-set-key (kbd "C-c x")  'smex))

;; smart-tabs
(use-package smart-tabs-mode :ensure t :config (smart-tabs-insinuate 'c 'c++ 'javascript))

;; smooth scrolling
(use-package smooth-scrolling :ensure t :config (smooth-scrolling-mode 1))

;; ivy
(use-package ivy :ensure t :init (ivy-mode))

;; swiper
(use-package swiper :ensure t)

;; counsel
(use-package counsel :ensure t)

;; ripgrep
(use-package ripgrep :ensure t)

;; magit
(use-package magit :ensure t)

;; rest client
(use-package restclient :ensure t)

;; diff-hl
(use-package diff-hl :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

;; rainbow parens
(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; circe
(use-package circe :ensure t
	:config
	(setq-default circe-network-options
							 '(("Mozilla"
									:use-tls t
									:nick "zovt"
									:host "irc.mozilla.org"
									:port 6697)
								 ("Snoonet"
									:use-tls t
									:nick "zovt"
									:host "irc.snoonet.org"
									:port 6697))))

;; theme
(use-package challenger-deep-theme :ensure t :init (load-theme 'challenger-deep t))

;; prog langs

;; lisp and emacs-lisp
(defun disable-tabs () "Disable tabs locally in a buffer." (setq-local indent-tabs-mode nil))
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; go
(use-package go-mode :ensure t
  :config
  (setq-default gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (setq-local compile-command "noti go test -v")))
  (define-key go-mode-map (kbd "C-c t d") 'godef-jump)
  (define-key go-mode-map (kbd "C-c t D") 'godef-jump-other-window)
  (define-key go-mode-map (kbd "C-c C")
    (lambda () (interactive)
      (start-process-shell-command "*go integration test*" "*go integration test*"
                                   (concat "cd " (locate-dominating-file default-directory ".git") " && noti make integration-test;"))
      (with-current-buffer "*go integration test*"
        (local-set-key (kbd "C-c C-c") (lambda () (interactive)
                                         (delete-process "*go integration test*")
                                         (kill-buffer "*go integration test*"))))
      (switch-to-buffer-other-window "*go integration test*"))))
(use-package company-go :ensure t :config (add-to-list 'company-backends 'company-go))

;; rust
(use-package racer :ensure t)
(use-package rust-mode :ensure t
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
	(define-key racer-mode-map (kbd "C-c t d") 'racer-find-definition)
	(define-key racer-mode-map (kbd "C-c t D") 'racer-describe))

;; exec path from shell
(use-package exec-path-from-shell :ensure t :init (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

;; code visuals
(setq-default tab-width 2)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; useful functions
;; editing
(defun scratch () "Create a new scratch buffer."
       (interactive)
       (switch-to-buffer "*scratch*")
       (lisp-interaction-mode))

;; keybinds
;; nicer ergonomics
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-c h") 'help)

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'."
  (interactive (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;; fix escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; window management
(global-set-key (kbd "C-c w n") 'windmove-down)
(global-set-key (kbd "C-c w p") 'windmove-up)
(global-set-key (kbd "C-c w f") 'windmove-right)
(global-set-key (kbd "C-c w b") 'windmove-left)
(global-set-key (kbd "C-c w d") 'delete-window)
(global-set-key (kbd "C-c w H") 'split-window-horizontally)
(global-set-key (kbd "C-c w V") 'split-window-vertically)
(global-set-key (kbd "C-c w F") 'make-frame)
(global-set-key (kbd "C-c w x") 'delete-frame)
(global-set-key (kbd "C-c w o") 'other-frame)

;; file finding
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c f") 'counsel-git)

;; swiper
(global-set-key (kbd "C-c C-c") 'swiper)

;; ripgrep
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c R") 'ripgrep-regexp)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; compile
(global-set-key (kbd "C-c c") 'compile)

;; modeline
(setq-default mode-line-format (list '(:eval (propertize " %b"))
                                     '(:eval (if (buffer-modified-p) "*" " "))
                                     '(:eval (propertize " ["))
                                     '(:eval mode-name)
                                     '(:eval (propertize "] "))
                                     '(:eval (propertize " {"))
                                     '(:eval minor-mode-alist)
                                     '(:eval (propertize "} "))))
