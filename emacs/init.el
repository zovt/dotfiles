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

;; Fullscreen
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Put text in center of screen
;; Taken from #Emacs, Naked
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
	   (* 120 (frame-char-width)))
	2))))
(bzg-big-fringe-mode 1)

;; Line numbers
(global-linum-mode 1)

;; Tramp settings
(setq tramp-default-method "ssh")

;; Git
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; Evil
(global-evil-leader-mode)
(evil-mode 1) ;; Enable evil
(define-key evil-normal-state-map ";" 'evil-ex) ;; Bind ";" to ":"
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent) ;; Make newline indent in insert mode
(setq evil-auto-indent t) ;; I don't think this actually does anything
(setq evil-shift-width 4) ;; Set indent width
