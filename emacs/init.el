(setq-default vendor-dir "~/.emacs.d/vendor")
(defun vendor-and-load-remote-file (remote local-name)
  "Automatically save REMOTE to LOCAL-NAME under `vendor-dir'."
  (if (not (file-exists-p vendor-dir))
      (make-directory vendor-dir))
  (let ((local-file (expand-file-name local-name vendor-dir)))
    (if (not (file-exists-p local-file))
        (url-copy-file remote local-file))
    (load-file local-file)))

(defun scratch () "Create a new scratch buffer."
       (interactive)
       (switch-to-buffer "*scratch*")
       (lisp-interaction-mode))

(defun bufname () "Interactive buffer name" (interactive) (message (buffer-name)))




(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visual-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq-default truncate-lines t)
(global-visual-line-mode 1)
(setq-default mode-line-format nil)

(global-font-lock-mode -1)
(deftheme zovt "My colors")
(let ((class '((class color) (min-colors 256))))
  (custom-theme-set-faces
   'zovt
   `(default ((,class (:foreground "white" :background "black"))))
   `(header-line ((,class (:foreground "white" :background "black" :overline "gray24"))))
   `(button ((,class (:foreground "deep sky blue"))))
   `(fringe ((,class (:background "black"))))
   `(region ((,class (:background "deep sky blue" :foreground "black"))))
   ))

(provide-theme 'zovt)
(provide 'zovt-theme)
(enable-theme 'zovt)


(set-face-font 'default "Ttyp0-16:antialias=false:hint=false")
(setq-default tab-width 2
              c-indent-offset 2
              c-default-style "k&r"
              indent-tabs-mode t)

(setq-default custom-file "~/.emacs.d/custom.el")
(setq-default backup-by-coping t
              delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2)
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))

(add-hook 'after-init-hook 'server-start)

(add-hook 'prog-mode-hook 'show-paren-mode)

(setq-default confirm-kill-emacs 'yes-or-no-p)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn (package-refresh-contents)
           (package-install 'use-package)))

(use-package smooth-scrolling :ensure t :config (smooth-scrolling-mode 1))

(use-package ivy
  :ensure t
  :init (ivy-mode)
  :config (setq-default ivy-use-virtual-buffers t
                        ivy-count-format "%d/%d"))
(use-package counsel :ensure t)

(use-package fiplr :ensure t)

(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode))

(use-package smart-tabs-mode
  :ensure t
  :config (smart-tabs-insinuate 'c 'c++ 'java 'javascript))

(vendor-and-load-remote-file "https://raw.githubusercontent.com/akrito/acme-mouse/master/acme-mouse.el" "acme-mouse.el")




(setq-default rust-indent-offset 2)
(add-hook 'rust-mode-hook (lambda () (setq-local indent-tabs-mode 't)))

(c-set-offset 'innamespace 0)
(electric-pair-mode)




(defun fd-get-matches (filename)
  (split-string
   (shell-command-to-string (concat "fd " filename))
   "\n" t))

(defun counsel-fd-func (str)
  (if (< (length str) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (concat "fd -c never " str))
    '("" "working...")))

(defun counsel-fd (&optional initial-input)
  (interactive)
  (ivy-read "fd: " #'counsel-fd-func
            :dynamic-collection t
            :initial-input initial-input
            :history 'counsel-fd-history
            :unwind 'counsel-delete-process
            :caller 'counsel-fd
            :action 'find-file))



(defun make-header-line-mouse-map (name command)
  (lexical-let ((name-b name)
                (command-b command)
                (buffer-b (current-buffer))
                (window-b (selected-window)))
    `(keymap (header-line keymap
                          (mouse-1 . ,(lambda ()
                                        (interactive)
                                        (save-excursion
                                          (set-buffer buffer-b)
                                          (select-window
                                           (if (window-live-p window-b)
                                               window-b
                                             (get-buffer-window buffer-b)))
                                          (funcall command-b))))
                          (mouse-2 . ,(lambda () (interactive) (delete-header-button name-b)))))))

(defun header-button (name command)
  (propertize name
              'face 'button
              'mouse-face 'mode-line-highlight
              'local-map (make-header-line-mouse-map name command)))

(defun add-header-button (name command)
  (interactive "sName: \nCCommand: ")
  (lexical-let ((command-b command))
    (setq-default header-buttons (append header-buttons `((,name .  ,(lambda () (interactive) (call-interactively command-b))))))))

(defun delete-header-button (name)
  (setq-default header-buttons (assq-delete-all name header-buttons)))

(defun create-header-line-format ()
  (mapcar (lambda (v)
            (concat (header-button (car v) (cdr v)) " "))
          header-buttons))

(setq-default header-buttons-default
              `(("Save" . save-buffer)
                ("Del" . delete-window)
                ("Kill" . ,(lambda () (interactive) (kill-this-buffer)))
                ("Open" . counsel-find-file)
                ("Find" . counsel-fd)
                ("Switch" . ivy-switch-buffer)
                ("Undo" . undo-only)
                ("Hori" . split-window-below)
                ("Vert" . split-window-right)
                ("Eval" . ,(lambda () (interactive) (call-interactively 'eval-last-sexp)))
                ("Grep" . counsel-rg)
                ("|" . ,(lambda () (interactive) (call-interactively 'add-header-button)))))

(defun reset-header-buttons ()
  (interactive)
  (setq-default header-buttons header-buttons-default))

(defun localize-header-buttons ()
  (interactive)
  (setq-local header-buttons header-buttons))

(reset-header-buttons)

(setq-default header-line-format
              (list "%*"
                    " "
                    '(:eval (create-header-line-format))))



(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c f") 'counsel-find-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c n") 'bufname)



(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
