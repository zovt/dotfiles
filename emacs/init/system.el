;;; system.el --- For system-related things
;;; Commentary:
;;; Holds all configuration for system-related things
;;; Code:

;; Change tramp settings if on windows
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))

;; Start emacsclient when opening GUI
(when window-system
  (server-start))

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

;; Kill old buffers automatically
(require 'midnight)
(midnight-delay-set 'midnight-delay "12:00am")
(setq midnight-period 10800)
(setq midnight-mode t)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(provide 'system)
;;; system.el ends here
