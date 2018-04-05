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
(use-package counsel)

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

(vendor-and-load-remote-file "https://raw.githubusercontent.com/akrito/acme-mouse/master/acme-mouse.el" "acme-mouse.el")




(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c f") 'counsel-find-file)
(global-set-key (kbd "C-c c") 'compile)




(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
