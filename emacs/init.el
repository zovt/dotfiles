;; basic visual
(set-face-font 'default "Iosevka-18")
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

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
(if (not (package-installed-p 'use-package)) (progn (package-refresh-contents) (package-install 'use-package)))

;; modes and plugins

;; electric pair mode
(electric-pair-mode)

;; undo tree
(use-package undo-tree :ensure t)

;; evil
(use-package evil :ensure t
  :config
	(evil-mode)
	(evil-global-set-key 'normal (kbd "<SPC>")
                       (lambda ()
                         (interactive)
                         (setq unread-command-events
                               (listify-key-sequence "\C-c"))))
  (evil-global-set-key 'normal ";" 'evil-ex)
  (evil-global-set-key 'normal "U" 'undo-tree-visualize))

;; smex
(use-package smex :ensure t :config (global-set-key (kbd "M-x")  'smex))

;; smart-tabs
(use-package smart-tabs-mode :ensure t :config (smart-tabs-insinuate 'c 'c++ 'javascript))

;; smooth scrolling
(use-package smooth-scrolling :ensure t :config (smooth-scrolling-mode 1))

;; prog langs

;; lisp
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))

;; go
(use-package go-mode :ensure t)

;; code visuals
(setq-default tab-width 2)
