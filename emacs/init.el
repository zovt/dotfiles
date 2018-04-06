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




(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visual-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq-default truncate-lines t)

(set-face-font 'default "Fixedsys Excelsior-16")
(setq-default tab-width 2)

(setq-default custom-file "~/.emacs.d/custom.el")
(setq-default back-directory-alist '(("" . "~/.emacs.d/backups")))

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




(use-package tao-theme :ensure t)
(load-theme 'tao-yin t)

(electric-pair-mode)

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

(vendor-and-load-remote-file "https://raw.githubusercontent.com/akrito/acme-mouse/master/acme-mouse.el" "acme-mouse.el")




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
    (setq-local header-buttons (append header-buttons `((,name .  ,(lambda () (interactive) (call-interactively command-b))))))
    (setq-local header-line-format (create-header-line-format))))

(defun delete-header-button (name)
  (setq-local header-buttons (assq-delete-all name header-buttons))
  (setq-local header-line-format (create-header-line-format)))

(defun create-header-line-format ()
  (mapcar (lambda (v)
            (concat (header-button (car v) (cdr v)) " "))
          header-buttons))

(setq-default header-buttons
              `(("Save" . save-buffer)
                ("Open" . counsel-find-file)
                ("Find" . fiplr-find-file)
                ("Switch" . ivy-switch-buffer)
                ("Undo" . undo)
                ("Del" . delete-window)
                ("Kill" . ,(lambda () (interactive) (kill-this-buffer)))
                ("Hori" . split-window-below)
                ("Vert" . split-window-right)
                ("Eval" . ,(lambda () (interactive) (call-interactively 'eval-last-sexp)))
                ("Grep" . counsel-rg)
                ("|" . ,(lambda () (interactive) (call-interactively 'add-header-button)))))

(setq-default header-line-format '(:eval (create-header-line-format)))



(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c f") 'counsel-find-file)
(global-set-key (kbd "C-c c") 'compile)




(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
