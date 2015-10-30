;;; mono.el --- set monospace fonts for some faces
;;; Commentary:
;;; Code:

(set-face-font 'org-table "Roboto Mono-12")
(set-face-font 'company-tooltip-common "Roboto Mono-12")
(set-face-font 'company-tooltip "Roboto Mono-12")

(defface mono
  '(()) "Monospace font" :group 'mono-faces)

(set-face-font 'mono "Roboto Mono-12")

(defun make-buffer-mono ()
  "Make the buffer font monospace."
  (interactive)
  (buffer-face-set 'mono))

;; modes that should be mono
(add-hook 'emacs-lisp-mode-hook 'make-buffer-mono)
(add-hook 'geiser-mode-hook 'make-buffer-mono)

(provide 'mono)
;;; mono.el ends here
