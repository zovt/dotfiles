(deftheme simple-dark
	"Simple, dark theme")

(let ((foreground "#FFF6E7")
			(dark-foreground "#DFD6C7")
			(background "#252533")
			(light-background "#454553")
			(doc-background "#353543")
			(light-rose "#FFABCB"))
	(custom-theme-set-faces
	 'simple-dark

	 ;; generic
	 `(default ((t (:background ,background :foreground ,foreground))))
	 `(cursor ((t (:background ,foreground :foreground ,background))))
	 `(button ((t (:background ,background :foreground ,foreground :underline t))))
	 `(region ((t (:background ,foreground :foreground ,background))))
	 `(linum ((t (:background ,background :foreground ,foreground))))
	 `(font-lock-comment-face ((t (:background ,background :foreground ,light-rose :weight bold))))
	 `(font-lock-comment-delimiter-face ((t (:background ,background :foreground ,light-rose))))
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
	 `(hl-line ((t (:background ,light-background :foreground ,foreground))))

	 ;; fringe
	 `(fringe ((t (:background ,background :foreground ,foreground))))

	 ;; js2 mode
	 `(js2-function-param ((t (:background ,background :foreground ,foreground))))
	 `(js2-external-variable ((t (:background ,background :foreground ,foreground))))

	 ;; company
	 `(company-tooltip ((t (:background ,light-background :foreground ,foreground))))
	 `(company-tooltip-common ((t (:background ,light-background :foreground ,foreground))))
	 `(company-tooltip-selection ((t (:background ,light-rose :foreground ,foreground))))
	 `(company-tooltip-common-selection ((t (:background ,light-rose :foreground ,foreground))))
	 `(company-scrollbar-fg ((t (:background ,light-rose :foreground ,light-rose))))
	 `(company-scrollbar-bg ((t (:background ,dark-foreground :foreground ,dark-foreground))))
	 `(company-preview ((t (:background ,background :foreground ,foreground))))
	 `(company-preview-common ((t (:background ,background :foreground ,foreground))))
	 )
	)

(provide-theme 'simple-dark)
