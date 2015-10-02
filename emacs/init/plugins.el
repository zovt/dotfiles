;;; plugins.el --- plugin configuration
;;; Commentary:
;;; Code:

;;;; Usability plugins
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

;;; Smex
(autoload 'smex "smex" "Nice M-x replacement" t)

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
(autoload 'helm-gtags-mode "helm-gtags" "Helm GTags mode" t)
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

;; Helm swoop
(setq helm-swoop-split-direction 'split-window-horizontally)

;;; Undo tree
(global-undo-tree-mode)

;;;; Coding plugins
;;; Electric pair mode
(electric-pair-mode)

;;; Semantic
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;;; Company
;; initialize company
(autoload 'global-company-mode "company" "Company mode" t)
(add-hook 'after-init-hook 'global-company-mode)
;; Set up backends
(setq company-backends '(company-clang company-ghc company-go company-semantic
				       company-gtags company-c-headers
				       company-cmake company-files company-elisp
				       company-auctex company-tern company-css
				       company))

;;; Flycheck
(autoload 'global-flycheck-mode "flycheck" "Flycheck mode" t)
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
(autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "Colored Parens" t)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)

;;; Aggressive Indent Mode
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)

;;; Rainbow Mode
(autoload 'rainbow-mode "rainbow-mode" "Rainbow Mode" t)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)

;;;; Modes
;;; JS2 Mode
(autoload 'js2-mode "js2-mode" "JS2 Mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

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

;;; Web Mode
(autoload 'web-mode "web-mode" "Web Mode" t)
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
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; Cabal settings
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(setq haskell-tags-on-save t)

;;; CSS Mode
(setq css-indent-offset 2) ;; Change default css mode offset

;;; Go Mode
(require 'go-mode)
(add-hook 'go-mode-hook (lambda ()
			  (add-hook (make-local-variable 'before-save-hook)
				    'gofmt-before-save)
			  (setq tab-width 2)
			  (require 'flymake-go)))

;;; Geiser
(setq geiser-active-implementations '(racket))

;;;; Project Management Plugins
;;; Projectile
(require 'projectile)
(projectile-global-mode)
(when (eq system-type 'windows-nt)
  (setq projectile-indexing-method 'alien))
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'grizzl)
(require 'helm-projectile)
(helm-projectile-on)

;;;; Org Mode
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
;; Turn on visual line mode
(add-hook 'org-mode-hook 'visual-line-mode)

(provide 'plugins)
;;; plugins.el ends here
