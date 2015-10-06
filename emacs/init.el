;;; init.el --- My init.el
;;; Commentary:
;;; Code:
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
