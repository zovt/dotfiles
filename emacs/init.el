;;; init.el --- My init.el
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/init/")

(require 'system)
(require 'packaging)
(require 'visual)
(require 'code)
(require 'plugins)
(require 'keybindings)
(load custom-file)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
