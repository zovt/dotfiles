;; visual
(set-face-font 'default "Hack-12")
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visual-bell 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(global-linum-mode 1)

;; change the custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; set up packages
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade"
				 . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn (package-refresh-contents)
	   (package-install 'use-package)))

;; turn on electric-pair-mode
(electric-pair-mode)

;; begin packages
;; ample theme
(use-package ample-theme
  :ensure t
  :config
  (load-theme 'ample-flat))

;; evil
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-global-set-key 'normal (kbd "<SPC>")
		       (lambda ()
			 (interactive)
			 (setq unread-command-events
			       (listify-key-sequence "\C-c"))))
  (evil-global-set-key 'normal ";" 'evil-ex)
  (evil-global-set-key 'normal "U" 'undo-tree-visualize))

;; undo-tree
(use-package undo-tree
  :ensure t)

;; smex
(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

;; company
(use-package company
  :ensure t
  :config
  (setq company-backends '(company-clang company-elisp company-dabbrev company-dabbrev-code))
  (global-company-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; rainbow-delimiters-mode
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
  (add-hook 'c++-mode-hook 'rainbow-delimiters-mode-enable))

;; org-mode
(setq org-log-done 'time)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; minimap
(use-package minimap
  :ensure t
  :config
  (setq minimap-window-location 'right))

;; smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

;; aggressive-indent-mode
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

;; fiplr
(use-package fiplr
  :ensure t)

;; keybinds
;; fix escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; window management
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w l") 'windmove-left)
(global-set-key (kbd "C-c w h") 'windmove-right)
(global-set-key (kbd "C-c w q") 'delete-window)

;; buffers
(global-set-key (kbd "C-c b n") 'next-buffer)
(global-set-key (kbd "C-c b p") 'previous-buffer)

;; minimap
(global-set-key (kbd "C-c t m") 'minimap-mode)

;; toggle aggresive indentation
(global-set-key (kbd "C-c t a") 'aggressive-indent-mode)

;; fiplr
(global-set-key (kbd "C-c f") 'fiplr-find-file)
