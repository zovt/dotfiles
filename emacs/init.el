;;; init.el --- My emacs configuration
;;; Commentary:
;;; This is my emacs configuration. I have moved from using an org-tangle set
;;; up because it was kind of obnoxious. So here is the new init.el, with
;;; everything commented in-line so that it is organized and whatnot.
;;; Code:

;;;; Helper functions
(defun check-executable (exe)
  "Check if EXE exists in path, and warn if not."
  (unless (executable-find exe)
    (warn-executable-not-found exe)))

(defun warn-executable-not-found (exe)
  "Warn that EXE wasn't found in path."
  (read-event
   (concat "Couldn't find '" exe "', please install it, the press a key"))
  nil)

(defun zovt-install-packages ()
  "Install packages."
  (interactive)
  (dolist (p zovt-packages)
    (quelpa p)))

(defun indent-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (point-to-register ?m)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (jump-to-register ?m))

;;;; Basic configuration that doesn't require any external packaging
;; Disable cursor blinking
(blink-cursor-mode 0)

;; Disable startup messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Turn off bell
(setq visible-bell t)

;; Get rid of GUI nonsense
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Set font based on what's available
(cond
 ((find-font (font-spec :name "Meslo LG S"))
  (set-face-font 'default "Meslo LG S-11"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-face-font 'default "Inconsolata-13"))
 ((find-font (font-spec :name "Fira Code"))
  (set-face-font 'default "Fira Code-12"))
 ((find-font (font-spec :name "courier"))
  (set-face-font 'default "courier-12")))

;; Enable line numbering
(global-linum-mode)

;; Change tramp settings if on windows
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

;; Start emacsclient when opening GUI
(when window-system
  (server-start))

;; Use heretical tabs for indent
(setq indent-tabs-mode t)
(setq tab-width 2)

;; Backups
(unless (file-accessible-directory-p "~/.emacs.d/.saves")
  (shell-command "mkdir ~/.emacs.d/.saves"))

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/.saves"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.saves/" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 6
      version-control t)

;;;; Packages
;; Set up Packaging
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade"
				 . "http://marmalade-repo.org/packages/") t)
;; Check if hg is installed
(check-executable "hg")

;; Install quelpa
(package-initialize)
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)
    (package-refresh-contents)
    (setq package-check-signature nil)
    (package-install 'let-alist)))

;; My package list for quelpa
(setq zovt-packages
      '(;; Themes
	material-theme
	monokai-theme
	solarized-theme
	;; Modes
	flycheck flymake-hlint org cmake-mode js2-mode
	auctex web-mode haskell-mode arduino-mode evil ghc-mod
	;; Utilities
	smex helm helm-gtags tern rainbow-delimiters
	powerline aggressive-indent undo-tree magit
	ace-jump-mode hydra helm-swoop yasnippet
	(rainbow-mode :url
		      "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el"
		      :fetcher url)
	;; Company
	company company-c-headers company-tern company-ghc))

;; Install packages
(unless (file-exists-p "~/.emacs.d/init-done")
  (zovt-install-packages)
  (shell-command "touch ~/.emacs.d/init-done"))


;;;; Theme
(load-theme 'solarized-light)

;;;; Plugins and modes
;;; Electric pair mode
(electric-pair-mode)
;; Add some pairings
(setq electric-pair-pairs '((?\' . ?\')
			    (?\" . ?\")
			    (?\< . ?\>)))
;; Remove certain pairs in some modes
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (setq electric-pair-pairs
			   (assq-delete-all '?\' electric-pair-pairs))))
(add-hook 'web-mode-hook
	  (lambda () (setq electric-pair-pairs
			   (assq-delete-all '?\< electric-pair-pairs))))

;;; Org Mode
;; What to log when finished a task by default
(setq org-log-done 'time)
;; Set agenda file
(when (file-exists-p "~/dotfiles/documents/calendar.org.gpg")
  (setq org-agenda-files (list "~/dotfiles/documents/calendar.org.gpg")))
;; Set todo keywords
(setq org-todo-keywords '((type "BUG(b)" "IN-PROGRESS" "WAITING" "|" "FIXED(f@)")
			  (type "SUGGESTION(s)" "ENHANCEMENT(e)" "|" "ADDED(a@)")
			  (type "GOAL(g)" "|" "DONE(d!)")
			  (type "|" "CANCELED(c@)")
			  (type "TODO" "IN-PROGRESS" "WAITING" "|"
				"DONE(d!)")))
;; Set export backends
(setq org-export-backends '(ascii md html odt org latex man))
;; Set up time tracking
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;; Smex
(require 'smex)

;;; Helm
(require 'helm)
(require 'helm-config)
(setq helm-mode-handle-completion-in-region nil)
;; Set helm prefix key
(setq helm-command-prefix-key "C-c h")
;; Helm options
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t)
(helm-mode 1)
;; Helm gtags
(require 'helm-gtags)
(setq helm-gtags-prefix-key (kbd "C-c g"))
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-suggested-key-mapping t)
;; Helm-gtags hooks
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;;; Semantic
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;;; Company
;; initialize company
(add-hook 'after-init-hook 'global-company-mode)
;; Set up backends
(setq company-backends '(company-clang company-ghc company-semantic
				       company-gtags company-c-headers
				       company-cmake company-files company-elisp
				       company-auctex company-tern company-css
				       company))
;;; C Indentation Mode
(setq c-default-style "user")

;;; JS2 Mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; Tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;;; Flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda ()
			   (setq flycheck-clang-standard-library "libc++")
			   (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c-mode-hook (lambda ()
			 (setq flycheck-clang-standard-library "libc")))
;; Fix things on windows
(if (eq system-type 'windows-nt)
    (progn
      (add-hook 'c++-mode-hook
		(lambda ()
		  (setq flycheck-clang-include-path
			(list
			 (expand-file-name "C:/msys64/mingw64/include")
			 (expand-file-name "C:/msys64/mingw64/x86_64-w64-mingw32/include")))))
      (add-hook 'c-mode-hook
		(lambda ()
		  (setq flycheck-clang-include-path
			(list
			 (expand-file-name "C:/msys64/mingw64/include")
			 (expand-file-name
			  "C:/msys64/mingw64/x86_64-w64-mingw32/include")))))))

;;; Rainbow delimiters
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)

;;; Powerline
(require 'powerline-themes)
(setq powerline-default-separator 'chamfer)
(powerline-center-evil-theme)

;;; Auctex
;; Settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-master nil)
;; Hooks
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Reftex
(setq reftex-plug-into-AUCTeX t)

;;; Aggressive Indent Mode
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)

;;; Undo tree
(global-undo-tree-mode)

;;; Ace Jump

;;; Web Mode
;; Autoload for .html and .php
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; Indentation
(defvar web-mode-enable-tab-indentation t)
(add-hook 'web-mode-hook (lambda ()
			   (setq web-mode-markup-indent-offset 2)
			   (setq web-mode-css-indent-offset 2)
			   (setq web-mode-code-indent-offset 2)))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;; Haskell Mode
;; Enable indentation
(add-hook 'haskell-mode-hook (lambda () 'turn-on-haskell-indentation))
;; Cabal settings
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(setq haskell-tags-on-save t)
;; Ghc-mod
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;; Evil
(evil-mode 1)
;; Ignore some modes
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)
;; Use SPC as "leader"
(evil-global-set-key 'normal (kbd "<SPC>")
		     (lambda ()
		       (interactive)
		       (setq unread-command-events
			     (listify-key-sequence "\C-c"))))
;; Evil-mode specific keybinds
(evil-global-set-key 'visual "a" 'align-regexp)
(evil-global-set-key 'insert (kbd "C-e") (evil-move-end-of-line))
(evil-global-set-key 'insert (kbd "C-a") (evil-move-beginning-of-line))
(evil-global-set-key 'normal (kbd "C-a") (evil-move-beginning-of-line))
(evil-global-set-key 'normal (kbd "C-e") (evil-move-end-of-line))
(evil-global-set-key 'normal ";" 'evil-ex)
(evil-global-set-key 'normal "U" 'undo-tree-visualize)

;;; Rainbow Mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)

;;; Yasnippet
(yas-global-mode 1)

;;;; Keybindings
(defhydra hydra-code (global-map "C-c c")
  "Code"
  ("=" indent-whole-buffer)
  ("i" imenu))

(defhydra hydra-movement (global-map "C-c m")
  "Movement"
  ("j" ace-jump-mode)
  ("s" helm-swoop))

(defhydra hydra-editing (global-map "C-c e")
  "Editing"
  ("r" align-regexp)
  ("u" undo-tree-visualize))

(defhydra hydra-git (global-map "C-c g")
  "Git"
  ("s" magit-status)
  ("c" magit-commit)
  ("C" magit-amend)
  ("P" magit-push)
  ("f" magit-fetch)
  ("F" magit-pull)
  ("i" magit-init)
  ("d" magit-diff-working-tree))

(defhydra hydra-files (global-map "C-c f")
  "Files"
  ("f" find-file))

(defhydra hydra-buffers (global-map "C-c b")
  "Buffers"
  ("b" switch-to-buffer))

(defhydra hydra-windows (global-map "C-c w")
  "Windows"
  ("J" other-window)
  ("K" other-window -1)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("v" split-window-vertically)
  ("h" split-window-horizontally))

;; Special non-hydra binds
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c x") 'smex)

;;; Theme
(load-theme 'solarized-light)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
