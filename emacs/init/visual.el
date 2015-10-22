;;; visual.el --- Configuration for the visual side of emacs
;;; Commentary:
;;; This is where I keep any visual-related functions.
;;; Code:

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Get rid of startup message
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Turn off bell
(setq visible-bell t)

;; Get rid of GUI nonsense
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Set font based on what's available
(cond
 ((find-font (font-spec :name "Roboto"))
  (set-face-font 'default "Roboto-12"))
 ((find-font (font-spec :name "Hack"))
  (set-face-font 'default "Hack-12"))
 ((find-font (font-spec :name "Roboto Mono"))
  (set-face-font 'default "Roboto Mono-12"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-face-font 'default "Inconsolata-13"))
 ((find-font (font-spec :name "Meslo LG S"))
  (set-face-font 'default "Meslo LG S-11"))
 ((find-font (font-spec :name "Fira Code"))
  (set-face-font 'default "Fira Code-12"))
 ((find-font (font-spec :name "courier"))
  (set-face-font 'default "courier-12")))

;; Set monospace fonts
(set-face-font 'org-table "Roboto Mono-11")
(set-face-font 'company-tooltip-common "Roboto Mono-11")
(set-face-font 'company-tooltip "Roboto Mono-11")

;;;; Theme
(load-theme 'molokai t)

;;; Modeline
(setq powerline-default-separator 'chamfer)
(require 'spaceline-config)
(spaceline-spacemacs-theme)

(provide 'visual)
;; visual.el ends here 
