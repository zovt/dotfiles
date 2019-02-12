(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visual-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(pixel-scroll-mode 1)
(blink-cursor-mode 0)
(setq-default truncate-lines t)
(setq-default auto-hscroll-mode 'current-line)

(set-face-font 'default "Iosevka Term-17:weight=medium")
(setq-default tab-width 8
              c-indent-offset 8
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
   `(default ((,class ,default-face)))
   `(font-lock-doc-face ((,class ,default-face)))
   `(font-lock-type-face ((,class ,default-face)))
   `(font-lock-builtin-face ((,class ,default-face)))
   `(font-lock-function-name-face ((,class ,default-face)))
   `(font-lock-variable-name-face ((,class ,default-face)))
   `(font-lock-constant-face ((,class ,default-face)))
   `(font-lock-comment-face ((,class (:foreground "black" :background "white" :slant italic)))
   `(font-lock-string-face ((,class ,default-face)))
   `(font-lock-keyword-face ((,class ,default-face))))))
(provide-theme 'zovt)
(provide 'zovt-theme)
(enable-theme 'zovt)

(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))

(add-hook 'after-init-hook 'server-start)

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq-default confirm-kill-emacs 'yes-or-no-p)

(global-set-key "\C-t" 'hippie-expand)

(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
