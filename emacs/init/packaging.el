;;; packaging.el -- My configuration for packages
;;; Commentary:
;;; This file configures basic packaging related things
;;; Code:

;; My packages
(defvar zovt-packages
  '(ample-theme
    soothe-theme
    material-theme
    molokai-theme
    ;; Modes
    flycheck flymake-hlint org cmake-mode js2-mode auctex web-mode
    haskell-mode arduino-mode evil ghc go-mode flymake-go
    geiser quack markdown-mode
    ;; Utilities
    smex helm helm-gtags rainbow-delimiters powerline aggressive-indent
    undo-tree magit ace-jump-mode hydra helm-swoop projectile
    grizzl helm-projectile exec-path-from-shell spaceline fancy-battery
    ;; Company
    company company-c-headers company-tern company-ghc))

;; Install packages function
(defun zovt-install-packages ()
  "Install packages."
  (interactive)
  (dolist (p zovt-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Set up Packaging
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade"
				 . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(if (not (package-installed-p 'ample-theme))
    (progn (package-refresh-contents)))

(zovt-install-packages)

(provide 'packaging)
;;; packaging.el ends here
