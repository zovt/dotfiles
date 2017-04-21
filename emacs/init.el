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

;; company
(use-package company :ensure t
  :config
  (setq-default company-idle-delay 0.1)
  (setq-default company-minimum-prefix-length 2)
  (add-hook 'after-init-hook 'global-company-mode))

;; flycheck
(use-package flycheck :ensure t :init (global-flycheck-mode))

;; smex
(use-package smex :ensure t :config (global-set-key (kbd "M-x")  'smex))

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

;; theme
(use-package leuven-theme :ensure t :config (load-theme 'leuven))

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
(use-package rust-mode :ensure t)

;; exec path from shell
(use-package exec-path-from-shell :ensure t :init (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

;; code visuals
(setq-default tab-width 2)

;; keybinds

;; fix escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; window management
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w q") 'delete-window)
(global-set-key (kbd "C-c w H") 'split-window-horizontally)
(global-set-key (kbd "C-c w V") 'split-window-vertically)

;; file finding
(global-set-key (kbd "C-c f") 'counsel-find-file)
(global-set-key (kbd "C-c F") 'counsel-git)

;; buffers
(global-set-key (kbd "C-c b") 'switch-to-buffer)

;; swiper
(global-set-key (kbd "C-c <SPC>") 'swiper)

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
                                     '(:eval minor-modo-alist)
                                     '(:eval (propertize "} "))))
