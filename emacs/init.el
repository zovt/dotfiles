;; require necessary packages
(require 'package)
(require 'cl-macs)

;; Enable melpa and Org-mode
(add-to-list 'package-archives
     '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

;; Install function
(defconst ragesalmon-config-packages
      '(evil
	evil-leader
	flycheck
	sublime-themes
	org
	projectile
	linum-relative
	magit
	smex
	helm
	js2-mode
	company
	stekene-theme))
(dolist (p ragesalmon-config-packages)
  (if (not (package-installed-p p))
      (package-install p)))

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Don't use unecessary messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Turn off bell
(setq visible-bell t)

(setq inhibit-startup-echo-area-message "mondieu")

;; Get rid of scroll bar
(scroll-bar-mode 0)

;; Get rid of gui things
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Set theme location
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'stekene-dark t)

;; Highlight current line
(global-hl-line-mode t)

;; Fullscreen
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Line numbers
(global-linum-mode 1)

;; Tramp settings
(setq tramp-default-method "plink")

;; Delete all but current buffer
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-c x") 'kill-other-buffers)

;; Evil
(global-evil-leader-mode)
(evil-mode 1) ;; Enable evil
(define-key evil-normal-state-map ";" 'evil-ex) ;; Bind ";" to ":"
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent) ;; Make newline indent in insert mode
(setq evil-auto-indent t) ;; I don't think this actually does anything
(setq evil-shift-width 4) ;; Set indent width

;; Org Mode
(setq org-log-done 'time)
(setq org-agenda-files (list "~/.emacs.d/org/school.org"
			     "~/.emacs.d/org/home.org"
			     "~/.emacs.d/org/life.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "RET") 'newline)))

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Company Mode
(add-hook 'after-init-hook 'global-company-mode)

;; Projectile
(projectile-global-mode)

;; Helm
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c b") 'helm-buffers-list)
(helm-mode 1)
