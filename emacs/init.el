;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visual-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(setq-default truncate-lines t)
(setq-default auto-hscroll-mode 'current-line)

(set-face-font 'default "Iosevka Term Slab-17:weight=medium")
(set-fontset-font t 'unicode "EmojiOne" nil 'prepend)
(setq-default tab-width 8
	      c-indent-offset 8
	      c-basic-offset 8
	      c-default-style '((other . "k&r"))
	      indent-tabs-mode t)
(setq-default backup-by-copying t
	      delete-old-versions t
	      kept-new-versions 6
	      kept-old-versions 2)

(deftheme zovt "My colors")
(let ((class '((class color) (min-colors 256)))
      (default-face '(:foreground "black" :background "white")))
  (custom-theme-set-faces
   'zovt
   `(default                        ((,class ,default-face)))
   `(font-lock-doc-face             ((,class ,default-face)))
   `(font-lock-type-face            ((,class ,default-face)))
   `(font-lock-builtin-face         ((,class ,default-face)))
   `(font-lock-function-name-face   ((,class ,default-face)))
   `(font-lock-string-face          ((,class ,default-face)))
   `(font-lock-variable-name-face   ((,class ,default-face)))
   `(font-lock-constant-face        ((,class ,default-face)))
   `(font-lock-comment-face         ((,class (:foreground "black" :background "white" :slant italic))))
   `(region                         ((,class (:foreground "black" :background "gray89"))))
   `(font-lock-string-face          ((,class ,default-face)))
   `(font-lock-keyword-face         ((,class ,default-face)))))
(provide-theme 'zovt)
(provide 'zovt-theme)
(enable-theme 'zovt)

(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))

(add-hook 'after-init-hook 'server-start)

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'fundamental-mode-hook (lambda () (local-set-key [tab] (insert-char ?\t))))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'c++-mode-hook (lambda ()
			   (setq c-noise-macro-names '("constexpr"))))

(setq-default confirm-kill-emacs 'yes-or-no-p)

(global-set-key "\C-t" 'hippie-expand)
(global-set-key "\C-c\ c" 'compile)

(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
