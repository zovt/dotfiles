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
    sublime-themes
    git-gutter
    org
    cmake-mode
    projectile
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
(toggle-frame-fullscreen)

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
(evil-define-key 'insert org-mode-map (kbd "RET") 'newline) ;; Disable auto-indent in org-mode
(setq evil-auto-indent t) ;; I don't think this actually does anything
(setq evil-shift-width 4) ;; Set indent width

;; Evil Leader Binds
(evil-leader/set-leader ",")
(evil-leader/set-key "h" 'previous-buffer)
(evil-leader/set-key "l" 'next-buffer)
(evil-leader/set-key "u" 'smex)
(evil-leader/set-key "a" 'org-agenda)
(evil-leader/set-key "x" 'kill-other-buffers)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "k" 'kill-buffer)

;; Org Mode
(setq org-log-done 'time)
(setq org-agenda-files (list "~/.emacs.d/org/school.org"
			     "~/.emacs.d/org/home.org"
			     "~/.emacs.d/org/Schedule.org"
			     "~/.emacs.d/org/life.org"))
(global-set-key (kbd "C-c a") 'org-agenda)

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

;; Git Gutter
(global-git-gutter-mode +1)

;; Customize modeline
(setq-default
 mode-line-format
	      '(
	       (:propertize " %m: " face font-lock-doc-face)
	       (:eval (when (buffer-modified-p)
			 (propertize "(MOD) "
				     'face 'font-lock-warning-face)))
	       (:propertize "%b " face font-lock-function-name-face)
	       (:propertize "[%02l : %02c] " face font-lock-keyword-face)
	       (:propertize "[%02p / %02I] " face font-lock-preprocessor-face)
	       (:eval (propertize (format-time-string "%H:%M:%S") 'face 'font-lock-builtin-face))
	       " ("
	       minor-mode-alist
	       " )"
	       (:propertize " %-" face font-lock-comment-face)))

;; Change save path
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 auto-save-file-name-transforms '((".*" "~/.saves/" t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
