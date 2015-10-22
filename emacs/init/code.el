;;; code.el --- Coding related configurations
;;; Commentary:
;;; This file configures basic coding related things
;;; Code:

;; Use heretical tabs for indent
(setq indent-tabs-mode t)
(setq c-basic-offset 6)
(setq tab-width 6)

;; Highlight lines over 80 chars
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;;; C Indentation Mode
(setq c-default-style "user")

(provide 'code)
;;; code.el ends here
