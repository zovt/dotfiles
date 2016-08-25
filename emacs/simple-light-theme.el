(deftheme simple-light
	"Simple, light theme")

(let ((background "#FFFDFD")
			(light-background "#DFDDDD")
			(foreground "#252533")
			(dark-foreground "#454553")
			(doc-background "#EFEDED")
			(brick "#e81c4f"))
	(custom-theme-set-faces
	 'simple-light

	 ;; generic
	 `(default ((t (:background ,background :foreground ,foreground))))
	 `(cursor ((t (:background ,foreground :foreground ,background))))
	 `(button ((t (:background ,background :foreground ,foreground :underline t))))
	 `(region ((t (:background ,foreground :foreground ,background))))
	 `(linum ((t (:background ,background :foreground ,foreground :weight normal))))
	 `(font-lock-comment-face ((t (:background ,background :foreground ,brick :weight bold))))
	 `(font-lock-comment-delimiter-face ((t (:background ,background :foreground ,brick))))
	 `(font-lock-builtin-face ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-keyword-face ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-string-face ((t (:background ,background :foreground ,foreground :underline t))))
	 `(font-lock-type-face ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-constant-face ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-function-name-face ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-variable-name-face ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-doc-face ((t (:background ,doc-background :foreground ,foreground))))
	 `(minibuffer-prompt ((t (:background ,background :foreground ,foreground))))
	 `(window-divider ((t (:background ,background :foreground ,background))))
	 `(window-divider-first-pixel ((t (:background ,background :foreground ,background))))
	 `(window-divider-last-pixel ((t (:background ,background :foreground ,background))))
	 `(vertical-border ((t (:background ,background :foreground ,background))))

	 ;; mode-line
	 `(mode-line ((t (:background ,background :foreground ,foreground))))
	 `(mode-line-inactive ((t (:background ,background :foreground ,dark-foreground))))

	 ;; line highlight
	 `(hl-line ((t (:background ,light-background))))

	 ;; fringe
	 `(fringe ((t (:background ,background :foreground ,foreground))))

	 ;; js2 mode
	 `(js2-function-param ((t (:background ,background :foreground ,foreground))))
	 `(js2-external-variable ((t (:background ,background :foreground ,foreground))))

	 ;; company
	 `(company-tooltip ((t (:background ,light-background :foreground ,foreground))))
	 `(company-tooltip-common ((t (:background ,light-background :foreground ,foreground))))
	 `(company-tooltip-selection ((t (:background ,brick :foreground ,foreground))))
	 `(company-tooltip-common-selection ((t (:background ,brick :foreground ,foreground))))
	 `(company-scrollbar-fg ((t (:background ,brick :foreground ,brick))))
	 `(company-scrollbar-bg ((t (:background ,dark-foreground :foreground ,dark-foreground))))
	 `(company-preview ((t (:background ,background :foreground ,foreground))))
	 `(company-preview-common ((t (:background ,background :foreground ,foreground))))

	 ;; web-mode
	 `(web-mode-html-tag-face ((t (:foreground ,foreground))))
	 `(web-mode-html-attr-name-face ((t (:foreground ,foreground))))
	 )
	)

(provide-theme 'simple-light)
