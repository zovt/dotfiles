;; visual
(set-face-font 'default "M+ 1m-14")
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; change the custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; indentation settings
(setq-default c-basic-offset 2
							tab-width 2
							indent-tabs-mode t)

;; change backup file location
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

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

;; my theme
(add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory "~/.emacs.d/")))

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

;; irony-mode
(use-package irony
	:ensure t
	:config
	(add-hook 'c++-mode-hook 'irony-mode)
	(if (string-equal system-type "windows-nt")
			(setq-default w32-pipe-read-delay 0)))

;; c++-mode
(add-to-list 'auto-mode-alist '("\\.tt\\'" . c++-mode))

;; cpputils-cmake
(use-package cpputils-cmake
	:ensure t
	:config
	(add-hook 'c++-mode-hook 'cppcm-reload-all))

;; company
(use-package company-irony
	:ensure t)

(use-package company-irony-c-headers
	:ensure t)

(use-package company-c-headers
	:ensure t)

(use-package company
	:ensure t
	:config
	(setq-default company-backends '(company-irony company-irony-c-headers company-c-headers company-clang company-elisp company-dabbrev-code company-dabbrev))
	(setq-default company-idle-delay 0.1)
	(setq-default company-minimum-prefix-length 1)
	(global-company-mode))

;; smart-tabs
(use-package smart-tabs-mode
	:ensure t
	:config
	(smart-tabs-insinuate 'c++ 'c 'java 'javascript))

;; rainbow-delimiters-mode
(use-package rainbow-delimiters
	:ensure t
	:config
	(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
	(add-hook 'c++-mode-hook 'rainbow-delimiters-mode-enable)
	(add-hook 'rust-mode-hook 'rainbow-delimiters-mode-enable))

;; org-mode
(setq-default org-log-done 'time)
(setq-default org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; minimap
(use-package minimap
	:ensure t
	:config
	(setq-default minimap-window-location 'right))

;; smooth-scrolling
(use-package smooth-scrolling
	:ensure t
	:config
	(smooth-scrolling-mode 1))

;; aggressive-indent-mode
(use-package aggressive-indent
	:ensure t
	:config
	(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

;; fiplr
(use-package fiplr
	:ensure t)

;; diminish
(use-package diminish
	:ensure t
	:config
	(diminish 'aggressive-indent-mode)
	(diminish 'company-mode)
	(diminish 'undo-tree-mode))

;; sr-speedbar
(use-package sr-speedbar
	:ensure t
	:config
	(setq-default speedbar-use-images nil)	
	(add-to-list 'evil-emacs-state-modes 'speedbar-mode)
	(add-hook 'speedbar-reconfigure-keymaps-hook
						(lambda ()
							(define-key speedbar-mode-map (kbd "h") 'windmove-left))))

;; haskell
(use-package haskell-mode
	:ensure t)

;; glsl-mode
(use-package glsl-mode
	:ensure t)

;; cmake-mode
(use-package cmake-mode
	:ensure t)

;; Rust
;; rust-mode
(use-package rust-mode
	:ensure t)

;; cargo
(use-package cargo
	:ensure t
	:config
	(add-hook 'rust-mode-hook 'cargo-minor-mode))

;; racer-emacs
(use-package racer
	:ensure t
	:config
	(setq-default racer-cmd "~/.cargo/bin/racer.exe")
	(setq-default racer-rust-src-path "~/rust-src/")
	(add-hook 'rust-mode-hook 'racer-mode)
	(add-hook 'racer-mode-hook 'eldoc-mode))

;; company-racer
(use-package company-racer
	:ensure t
	:config
	(add-to-list 'company-backends 'company-racer))


;; web-mode
(use-package web-mode
	:ensure t
	:config
	(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; js2-mode
(use-package js2-mode
	:ensure t
	:config
	(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
	(setq-default js2-basic-offset 2))

;; flycheck
(use-package flycheck
	:ensure t
	:config
	(add-hook 'c++-mode-hook (lambda ()
														 (interactive)
														 (setq-default flycheck-clang-language-standard "c++14")))
	(add-hook 'c-mode-hook (lambda ()
													 (interactive)
													 (setq-default flycheck-clang-language-standard "c11")
													 (setq flycheck-clang-include-path (list (expand-file-name "~/local/include")))))
	(add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
	(add-to-list 'flycheck-disabled-checkers 'javascript-jslint)
	(add-to-list 'flycheck-disabled-checkers 'javascript-jscs)
	(add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
	(add-to-list 'flycheck-checkers 'javascript-eslint)
	(flycheck-add-mode 'javascript-eslint 'js2-mode)
	(global-flycheck-mode))

;; undo-tree
(use-package undo-tree
	:ensure t
	:config
	(global-undo-tree-mode))

;; fish-mode
(use-package fish-mode
	:ensure t)

;; tuareg mode
(use-package tuareg
	:ensure t)

;; nlinum mode
(use-package nlinum
	:ensure t
	:config
	(global-nlinum-mode))

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

;; buffers
(global-set-key (kbd "C-c b n") 'next-buffer)
(global-set-key (kbd "C-c b p") 'previous-buffer)
(global-set-key (kbd "C-c b b") 'switch-to-buffer)

;; minimap
(global-set-key (kbd "C-c t m") 'minimap-mode)

;; toggle aggresive indentation
(global-set-key (kbd "C-c t a") 'aggressive-indent-mode)

;; fiplr
(global-set-key (kbd "C-c f") 'fiplr-find-file)

;; neotree
(global-set-key (kbd "C-c n") 'sr-speedbar-toggle)

;; swap ctrl-t and ctrl-x
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

;; customizing the modeline
(setq-default mode-line-format
							(list '(:eval (propertize " %b"))
										'(:eval (if (buffer-modified-p) "*" " "))
										'(:eval (propertize " ["))
										mode-name
										'(:eval (propertize "] "))
										'(:eval (propertize " {"))
										minor-mode-alist
										'(:eval (propertize "} "))))
