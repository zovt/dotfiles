;;; Init.el --- my emacs settings
;;; Commentary:
;;; This will set up all necessary things to make Emacs function the way I want it to
;;; Code:
;; ---------------- Packages and package settings ---------------------
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
    ;; themes
    stekene-theme
    moe-theme
    sublime-themes
    soothe-theme
    ample-theme
    tangotango-theme
    leuven-theme
    tango-plus-theme
    flatui-theme
    flatland-theme
    afternoon-theme
    purple-haze-theme
    birds-of-paradise-plus-theme
    gruvbox-theme
    ;; Modes
    flycheck
    org
    cmake-mode
    magit
    js2-mode
    yasnippet
    semantic
    function-args
    ;; Utilities
    smex
    helm
    helm-gtags
    helm-anything
    tern
    smartparens
    anything
    neotree
    rainbow-delimiters
    powerline
    ;; Company
    company
    company-c-headers
    company-tern
    ))
(dolist (p ragesalmon-config-packages)
  (if (not (package-installed-p p))
      (package-install p)))

;; --------------- General Configuration -----------------------

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
(require 'moe-theme)
(moe-dark)

;; Highlight current line
(global-hl-line-mode t)

;; Fullscreen
(if (eq system-type 'windows-nt)
    (toggle-frame-fullscreen)
  )

;; Set Font
(set-face-font 'default "InputMono-10")

;; Line numbers
(global-linum-mode 1)

;; Tramp settings
(defvar tramp-default-method "plink")

;; Start emacsclient when opening a GUI
(when window-system
  (server-start))

;; Make Git not slow
(if (eq system-type 'windows-nt)
    (progn
      (require 'vc)
      (remove-hook 'find-file-hooks 'vc-find-file-hook)
      (delete 'Git vc-handled-backends)))

;; Tab width
(setq tab-width 4)

;; Change save path
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 auto-save-file-name-transforms '((".*" "~/.saves/" t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Customize modeline
;; (setq-default mode-line-format '((:propertize " %m: " face font-lock-doc-face) (:eval (when (buffer-modified-p) (propertize "(MOD) " 'face 'font-lock-warning-face))) (:propertize "%b " face font-lock-function-name-face) (:propertize "[%02l : %02c] " face font-lock-keyword-face) (:propertize "[%02p / %02I] " face font-lock-preprocessor-face) (:eval (propertize (format-time-string "%H:%M:%S") 'face 'font-lock-builtin-face)) " (" minor-mode-alist " )" (:propertize " %-" face font-lock-comment-face))) (set-face-background 'mode-line "#434445")

;; --------------- Custom Functions -----------------
;; Delete all but current buffer
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-c x") 'kill-other-buffers)

;; ----------------- Plugin Configuration -----------------------
;; Evil
(global-evil-leader-mode)
(evil-mode 1) ;; Enable evil
(define-key evil-normal-state-map ";" 'evil-ex) ;; Bind ";" to ":"
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent) ;; Make newline indent in insert mode
(evil-define-key 'insert org-mode-map (kbd "RET") 'newline) ;; Disable auto-indent in org-mode
(defvar evil-auto-indent t) ;; I don't think this actually does anything
(defvar evil-shift-width 4) ;; Set indent width
;; Disable evil in ERC
(evil-set-initial-state 'erc-mode 'emacs)

;; Evil Leader Binds
(evil-leader/set-leader ",")
(evil-leader/set-key "h" 'previous-buffer)
(evil-leader/set-key "l" 'next-buffer)
(evil-leader/set-key "u" 'smex)
(evil-leader/set-key "a" 'org-agenda)
(evil-leader/set-key "x" 'kill-other-buffers)
(evil-leader/set-key "o" 'helm-buffers-list)
(evil-leader/set-key "k" 'kill-buffer)
(evil-leader/set-key "e" 'other-window)
(evil-leader/set-key "w" 'whitespace-mode)
(evil-leader/set-key "f" 'anything)
(evil-leader/set-key "n" 'neotree-toggle)

;; Org Mode
(defvar org-log-done 'time)
(defvar org-agenda-files (list "~/.emacs.d/org/school.org"
			       "~/.emacs.d/org/home.org"
			       "~/.emacs.d/org/Schedule.org"
			       "~/.emacs.d/org/life.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(defvar org-todo-keywords '((type "BUG(b)" "|" "FIXED(f@)")
			    (type "SUGGESTION(s)" "ENHANCEMENT(e)" "|" "ADDED(a@)")
			    (type "GOAL(g)" "|" "DONE(d!)")
			    (type "|" "CANCELED(c@)")
			    (type "TODO" "|" "DONE(d!)")))

(defvar
  org-export-backends '(ascii
			md
			html
			odt
			org
			latex
			man
			)
  )

(add-hook 'org-mode-hook (lambda() 'toggle-truncate-lines))

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Helm
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c b") 'helm-buffers-list)
(defvar helm-mode-handle-completion-in-region nil)

(defvar helm-command-prefix-key "C-c h")

(require 'helm-config)
(defvar helm-google-suggest-use-curl-p t)
(defvar helm-quick-update t)
(defvar helm-idle-delay 0.01)
(defvar helm-input-idle-delay 0.01)
(defvar helm-ff-search-library-in-sexp t)

(defvar helm-split-window-default-side 'other )
(defvar helm-split-window-in-side-p t )
(defvar helm-buffers-favorite-modes '(picture-mode artist-mode))
(defvar helm-candidate-number-limit 200 )
(defvar helm-M-x-requires-pattern 0     )
(defvar helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") )
(defvar helm-ff-file-name-history-use-recentf t)
(defvar helm-move-to-line-cycle-in-source t )
(defvar ido-use-virtual-buffers t)
(defvar helm-buffers-fuzzy-matching t)

(helm-mode 1)

;; Helm gtags
(defvar helm-gtags-prefix-key "\C-cg")

(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-suggested-key-mapping t
 )

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Yasnippet
(yas-global-mode 1)

;; Semantic
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; Function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map [(tab)] 'moo-complete)
(define-key c++-mode-map [(tab)] 'moo-complete)

;; Company
(add-hook 'after-init-hook 'global-company-mode)
(defvar company-backends '(
			   company-clang
			   company-semantic
			   company-gtags
			   company-c-headers
			   company-cmake
			   company-files
			   company-elisp
			   company-tern
			   company-css
			   company
			   ))
(defvar company-idle-delay 0.2)

;; C Indentation mode
(defvar c-defualt-style "linux")

;; Smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

(defun ragesalmon-newline-sp (&rest _ignored)
  "Indent properly when enter is pressed inside of curly braces."
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  )

(sp-local-pair 'c-mode "{" nil  :post-handlers '((ragesalmon-newline-sp "RET")))
(sp-local-pair 'c++-mode "{" nil  :post-handlers '((ragesalmon-newline-sp "RET")))
(sp-local-pair 'js2-mode "{" nil  :post-handlers '((ragesalmon-newline-sp "RET")))
(sp-local-pair 'css-mode "{" nil  :post-handlers '((ragesalmon-newline-sp "RET")))
(setq sp-autoskip-closing-pair (quote always))

;; Js2 Mode
(autoload 'js2-mode "js2-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Tern
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Anything
(require 'anything-match-plugin)
(require 'anything-config)

;; Neotree
(require 'neotree)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Rainbow delimiters
(global-rainbow-delimiters-mode)

;; Encryption
(epa-file-enable)

;; Powerline
(require 'powerline)
(setq-default powerline-default-separator 'contour)
(setq-default mode-line-format
	      '("%e"
		(:eval
		 (let* ((active (powerline-selected-window-active))
			(mode-line (if active 'mode-line 'mode-line-inactive))
			(face1 (if active 'powerline-active1 'powerline-inactive1))
			(face2 (if active 'powerline-active2 'powerline-inactive2))
			(middle-face (if active 'powerline-inactive1 'powerline-active2))
			(separator-left (intern (format "powerline-%s-%s" powerline-default-separator (car powerline-default-separator-dir))))
			(separator-right (intern (format "powerline-%s-%s" powerline-default-separator (cdr powerline-default-separator-dir))))
			(lhs (list (if (buffer-modified-p) (powerline-raw " (MOD)") (powerline-raw " -----"))
				   (powerline-raw " ")
				   (powerline-major-mode)
				   (powerline-raw ":")
				   (powerline-buffer-id nil 'l)
				   (powerline-buffer-size nil 'l)
				   (powerline-raw " ")
				   (funcall separator-left mode-line face2)
				   (powerline-raw " " face2 face1)
				   (powerline-raw (capitalize (symbol-name evil-state)) face2 face1)
				   (powerline-raw " " face2 face1)
				   (funcall separator-left face2 face1)
				   (powerline-raw " " face1 face2)
				   (powerline-minor-modes face1 face2)
				   (powerline-raw " " face1 face2)
				   (funcall separator-left face1 middle-face)
				   ))
			(rhs (list
			      (funcall separator-right middle-face face2)
			      (powerline-raw " " face2 'r)
			      (powerline-raw (format-time-string "%H:%M:%S") face2 'r)
			      (powerline-raw " " face2 'r)
			      (funcall separator-right face2 face1)
			      (powerline-hud face2 face1)
			      ))
			)
		   (concat (powerline-render lhs)
			   (powerline-fill middle-face (powerline-width rhs))
			   (powerline-render rhs))
		   )
		 )
		)
	      )

(provide 'init)
;;; init.el ends here
