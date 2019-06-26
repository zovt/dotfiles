(setq-default custom-file "~/.emacs.d/custom.el")

;; package setup --
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; ----------------

;; install needed packages --
(defun install-if-needed (package-atom)
  (interactive)
  (if (not (package-installed-p package-atom))
      (package-install package-atom)
    (require package-atom)))

(install-if-needed 'direx)
(install-if-needed 'diminish)
(install-if-needed 'magit)
(install-if-needed 'ripgrep)
;; --------------------------

;; visual --
(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visual-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(setq-default truncate-lines t)
(setq-default mouse-autoselect-window -0.12)

(set-face-font 'default "M+ 1m 14")
(set-fontset-font t 'unicode "EmojiOne" nil 'prepend)
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
   `(mode-line                      ((,class (:foreground "white" :background "black" :box "black"))))
   `(mode-line-inactive             ((,class (:foreground "black" :background "white" :box "black"))))
   `(fringe                         ((,class (:background "white"))))
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

(diminish 'abbrev-mode)

(fringe-mode 20)
;; ---------

(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; hooks --
(add-hook 'after-init-hook 'server-start)

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'c++-mode-hook (lambda ()
                                (setq c-noise-macro-names '("constexpr"))))
;; --------

;; dedicated windows --
(defun mark-window-dedicated ()
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "Window marked dedicated"))

(defun mark-window-not-dedicated ()
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "Window unmarked dedicated"))

(global-set-key "\C-c\ d" 'mark-window-dedicated)
(global-set-key "\C-c\ D" 'mark-window-not-dedicated)
;; --------------------

;; indentation ----
(defun newline-indent-match-previous ()
  (interactive)
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column col)))

(global-set-key "\t" 'tab-to-tab-stop)
(global-set-key (kbd "<backtab>") 'indent-according-to-mode)
(global-set-key (kbd "RET") 'newline-indent-match-previous)

(setq-default backward-delete-char-untabify-method nil)
(setq-default tab-width 8
              c-indent-offset 8
              c-basic-offset 8
              c-default-style '((other . "k&r"))
              indent-tabs-mode t)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(electric-indent-mode 0)
;; ----------------

(setq-default confirm-kill-emacs 'yes-or-no-p)

;; other keybinds --
(global-set-key "\C-t" 'hippie-expand)
(global-set-key "\C-c\ c" 'compile)
;; -----------------

(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
