;;; init.el -- My init.el
;;; Commentary:
;;; This is my `init.el'.  I try to keep it lean and useful, and
;;; use defaults by... default.  This file will change when I feel it necessary.
;;; It is NOT indicative of Emacs best practice, but it works for me.

;;; Code:

;; basic visual
(set-face-font 'default "Iosevka-15")
(global-prettify-symbols-mode)
(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visible-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq-default truncate-lines t)

;; custom file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; backups
(setq-default backup-directory-alist '(("" . "~/.emacs.d/backups")))

;; start server
(add-hook 'after-init-hook 'server-start)

;; confirm kill emacs
(setq-default confirm-kill-emacs 'yes-or-no-p)

;; repeat mark command pop
(setq-default set-mark-command-repeat-pop t)

;; packaging
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn (package-refresh-contents) (package-install 'use-package)))

;; ghetto shim for loading code which is on github but not melpa
(setq-default vendor-dir "~/.emacs.d/vendor")
(defun vendor-and-load-remote-file (remote local-name)
  "Automatically save REMOTE to LOCAL-NAME under `vendor-dir'."
  (if (not (file-exists-p vendor-dir))
      (make-directory vendor-dir))
  (let ((local-file (expand-file-name local-name vendor-dir)))
    (if (not (file-exists-p local-file))
        (url-copy-file remote local-file))
    (load-file local-file)))

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
  (define-key company-active-map (kbd "C-w") 'kill-region-or-backward-kill-word)
  (add-hook 'after-init-hook 'global-company-mode))

;; flycheck
(use-package flycheck :ensure t :init (global-flycheck-mode))

;; smart-tabs
(use-package smart-tabs-mode :ensure t :config (smart-tabs-insinuate 'c 'c++ 'javascript))

;; smooth scrolling
(use-package smooth-scrolling :ensure t :config (smooth-scrolling-mode 1))

;; ivy
(use-package ivy :ensure t :init (ivy-mode)
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-count-format "%d/%d"))

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

;; Multiple cursors
(use-package multiple-cursors :ensure t
  :config
  (global-set-key (kbd "C-S-t") 'mc/mark-next-like-this-symbol)
  (global-set-key (kbd "C-<") 'mc/mark-all-symbols-like-this))

;; theme
;; (use-package challenger-deep-theme :ensure t :init (load-theme 'challenger-deep t))
;; (use-package espresso-theme :ensure t :init (load-theme 'espresso t))
;; (use-package plan9-theme :ensure t :init (load-theme 'plan9 t))
;; (use-package color-theme-solarized :ensure t
;;   :init (load-theme 'solarized t)
;;   :config (setq-default frame-background-mode 'light))
(vendor-and-load-remote-file "https://raw.githubusercontent.com/zovt/simple.el/master/simple.el" "simple.el")
(enable-theme 'simple)

;; org mode
(require 'org nil t)
(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (setq-local fill-column 80)))
(setq-default org-todo-keywords '((sequence
                                   "TODO(t!)"
                                   "IN-PROGRESS(i!)"
                                   "BLOCKED(b@)"
                                   "REVIEW(r@)"
                                   "DONE(d!)"
                                   "CANCELED(c@)"
                                   )))
(bind-key (kbd "C-c C-,") 'org-todo org-mode-map)

;; exec path from shell
(use-package exec-path-from-shell :ensure t :init (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

;; ledger
(use-package ledger-mode :ensure t
  :config
  (setq-default ledger-mode-should-check-version nil
                ledger-report-links-in-register nil
                ledger-binary-path "hledger"))

;; ace-window
(use-package ace-window :ensure t :config (setq-default aw-dispatch-always t))

;; telephone-line
(use-package telephone-line :ensure t
  :config
  (setq-default telephone-line-height 30)
  (telephone-line-mode t))

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
                            (subword-mode 1)
                            (setq-local compile-command "noti go test")))
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
(use-package flycheck-rust :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
(use-package racer :ensure t)
(use-package rust-mode :ensure t
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook (lambda ()
                              (setq-local indent-tabs-mode t)
                              (setq-local tab-width 2)
                              (setq-local rust-indent-offset 2)))
  (setq-default rust-format-on-save t)
	(define-key racer-mode-map (kbd "C-c t d") 'racer-find-definition)
	(define-key racer-mode-map (kbd "C-c t D") 'racer-describe))

;; prose (the written word)
(vendor-and-load-remote-file "https://raw.githubusercontent.com/amperser/proselint/master/plugins/flycheck/flycheck-proselint.el"
                             "flycheck-proselint.el")

(vendor-and-load-remote-file "https://raw.githubusercontent.com/abingham/flycheck-vale/master/flycheck-vale.el"
                             "flycheck-vale.el")
(add-to-list 'flycheck-checkers 'vale)

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
;; M-x replacement
(global-set-key (kbd "C-c x") 'counsel-M-x)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; window management
(global-set-key (kbd "C-x o") 'ace-window)
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

;; editing
(global-set-key (kbd "C-S-k") 'kill-whole-line)
;; nicer ergonomics
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map (kbd "C-'") (kbd "C-c"))
(define-key key-translation-map (kbd "C-t") (kbd "C-x"))

(global-set-key (kbd "C-c h") 'help)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-;") 'save-buffer)
(global-set-key (kbd "C-,") 'transpose-chars)

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'."
  (interactive (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;; load local customizations
(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))

(provide 'init)
;;; init.el ends here
