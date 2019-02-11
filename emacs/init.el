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
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))

(add-hook 'after-init-hook 'server-start)

(add-hook 'prog-mode-hook 'show-paren-mode)

(setq-default confirm-kill-emacs 'yes-or-no-p)

(global-set-key "\C-t" 'hippie-expand)

(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))
