;; startup speed hacks
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun speedup-defer (time function)
  "Runs function with idle timer"
  (run-with-idle-timer time nil function))

(setq-default gc-cons-threshold (* 50 1000 1000))

;; package setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; install needed packages
(defun install-if-needed (package-atom)
  (interactive "Spackage: ")
  (if (not (package-installed-p package-atom))
      (package-install package-atom)
    (autoload package-atom (symbol-name package-atom) nil t)))

(install-if-needed 'direx)
(install-if-needed 'ripgrep)
(install-if-needed 'form-feed)
(install-if-needed 'magit)
(install-if-needed 'mood-line)

;; visual
(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visual-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(speedup-defer 0.1 (lambda () (scroll-bar-mode 0)))
(setq-default truncate-lines t)
(setq-default mouse-autoselect-window -0.12)

(set-face-font 'default "M+ 1m 15")
(set-face-font 'variable-pitch "Liberation Serif 15")
(set-fontset-font t 'unicode "EmojiOne" nil 'prepend)
(setq-default backup-by-copying t
              delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2)

(setq-default scroll-preserve-screen-position t
              scroll-conservatively 0
              maximum-scroll-margin 0.5
              scroll-margin 99999)

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
   `(font-lock-comment-face         ((,class (:foreground "black" :background "white" :family "Liberation Serif" :height 160))))
   `(region                         ((,class (:foreground "black" :background "gray89"))))
   `(font-lock-string-face          ((,class ,default-face)))
   `(font-lock-keyword-face         ((,class ,default-face)))))
(provide-theme 'zovt)
(provide 'zovt-theme)
(enable-theme 'zovt)

(define-globalized-minor-mode global-form-feed-mode form-feed-mode
  (lambda () (form-feed-mode 1)))
(global-form-feed-mode 1)

(speedup-defer 0.1 (lambda () (fringe-mode 20)))

(mood-line-mode)
(setq-default mode-line-format
              '((:eval
                 (mood-line-format
                  ;; Left
                  (format-mode-line
                   '((:eval (mood-line-segment-modified))
                     (:eval (concat (propertize "%b" 'face 'mode-line-buffer-id) ":%l"))))
                  ;; Right
                  (format-mode-line
                   '((:eval (mood-line-segment-major-mode))
                     (:eval (mood-line-segment-process))
                     " "))))))

;; hooks
(add-hook 'after-init-hook 'server-start)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'c++-mode-hook (lambda ()
                                (setq c-noise-macro-names '("constexpr"))))
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))

;; other
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq-default confirm-kill-emacs 'yes-or-no-p)
(setq-default custom-file "~/.emacs.d/custom.el")

;; dedicated windows
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

;; indentation
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

;; other keybinds
(global-set-key "\C-t" 'hippie-expand)
(global-set-key "\C-c\ c" 'compile)
(global-set-key "\C-c\ l" (lambda () (interactive) (insert-char ?\^L)))
(global-set-key "\C-c\ g" 'magit)


(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))

(speedup-defer 0.1 (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
