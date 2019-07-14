;; startup speed hacks
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun speedup-defer (time function)
  "Runs function with idle timer"
  (run-with-idle-timer time nil function))

(setq-default gc-cons-threshold (* 50 1000 1000))

;; package setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defun read-file-to-string (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(setq-default package-update-time-file "~/.emacs.d/package-update-time")
(defun refresh-package-contents-daily ()
  (if (not (file-exists-p package-update-time-file))
      (progn (make-empty-file package-update-time-file)
             (write-region "0" nil package-update-time-file)))
  (let ((last-update (string-to-number (read-file-to-string package-update-time-file)))
        (current-time (string-to-number (format-time-string "%s"))))
    (if (> (- current-time last-update) 86400)
        (progn (package-refresh-contents)
               (write-region (number-to-string current-time)
                             nil package-update-time-file)))))
(refresh-package-contents-daily)
(package-initialize)

;; install needed packages
(defun install-if-needed (package-atom)
  (interactive "Spackage: ")
  (if (not (package-installed-p package-atom))
      (package-install package-atom)))

(install-if-needed 'direx)
(install-if-needed 'ripgrep)
(install-if-needed 'form-feed)
(install-if-needed 'magit)
(install-if-needed 'mood-line)
(install-if-needed 'mu4e-alert)
(install-if-needed 'esup)
(install-if-needed 'undo-tree)
(install-if-needed 'ivy)
(install-if-needed 'flx)
(install-if-needed 'elfeed)
(install-if-needed 'centaur-tabs)

;; visual
(setq-default initial-scratch-message "")
(setq-default inhibit-startup-message t)
(setq-default visible-bell t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(speedup-defer 0.1 (lambda () (scroll-bar-mode 0)))
(setq-default truncate-lines t)
(setq-default mouse-autoselect-window -0.12)

(set-face-font 'default "Iosevka Term Slab 14")
(set-face-font 'variable-pitch "ETBembo 16")
(setq-default backup-by-copying t
              delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2)

(defun read-color (color-name)
  "Reads a color from my .colors file"
  (interactive "scolor name: ")
  (with-temp-buffer
    (insert-file-contents "~/.colors")
    (goto-char 0)
    (search-forward color-name)
    (forward-char 1)
    (setq min (point))
    (forward-word 1)
    (buffer-substring-no-properties min (point))))

(deftheme zovt "My colors")
(let* ((class '((class color) (min-colors 256)))
       (foreground (read-color "foreground"))
       (background (read-color "background"))
       (alt-background (read-color "alt-background"))
       (default-face `(:foreground ,foreground :background ,background)))
  (custom-theme-set-faces
   'zovt
   `(default                        ((,class ,default-face)))
   `(mode-line                      ((,class (:foreground ,background :background ,foreground))))
   `(mode-line-inactive             ((,class ,default-face)))
   `(vertical-border                ((,class (:foreground ,background :background ,foreground))))
   `(fringe                         ((,class ,default-face)))
   `(centaur-tabs-default           ((,class ,default-face)))
   `(centaur-tabs-selected          ((,class ,default-face)))
   `(centaur-tabs-unselected        ((,class (:foreground ,background :background ,foreground))))
   `(centaur-tabs-selected-modified ((,class (:background ,alt-background :foreground ,foreground))))
   `(centaur-tabs-unselected-modified ((,class (:foreground ,alt-background :background ,foreground))))
   `(centaur-tabs-close-selected    ((,class ,default-face)))
   `(centaur-tabs-close-unselected  ((,class (:foreground ,background :background ,foreground))))
   `(header-line                    ((,class (:foreground ,background :background ,foreground))))
   `(font-lock-doc-face             ((,class ,default-face)))
   `(font-lock-type-face            ((,class ,default-face)))
   `(font-lock-builtin-face         ((,class ,default-face)))
   `(font-lock-function-name-face   ((,class ,default-face)))
   `(font-lock-string-face          ((,class ,default-face)))
   `(font-lock-variable-name-face   ((,class ,default-face)))
   `(font-lock-constant-face        ((,class ,default-face)))
   `(font-lock-comment-face         ((,class ,(append default-face '(:slant italic)))))
   `(region                         ((,class (:foreground ,foreground :background ,alt-background))))
   `(show-paren-match               ((,class (:foreground ,background :background ,foreground))))
   `(show-paren-expression          ((,class (:foreground ,background :background ,foreground))))
   `(font-lock-string-face          ((,class ,default-face)))
   `(font-lock-keyword-face         ((,class ,default-face)))))
(provide-theme 'zovt)
(provide 'zovt-theme)
(enable-theme 'zovt)

(define-globalized-minor-mode global-form-feed-mode form-feed-mode
  (lambda () (form-feed-mode 1)))
(global-form-feed-mode 1)

(speedup-defer 0.1 (lambda () (fringe-mode 20)))

(mood-line-mode)
(setq-default mode-line-format
              '((:eval
                 (mood-line-format
                  ;; Left
                  (format-mode-line
                   '((:eval (mood-line-segment-modified))
                     (:eval (concat (propertize "%b" 'face 'mode-line-buffer-id) ":%l"))))
                  ;; Right
                  (format-mode-line
                   '((:eval (mood-line-segment-major-mode))
                     (:eval (mood-line-segment-process))
                     " "))))))

(centaur-tabs-mode t)
(centaur-tabs-headline-match)

;; hooks
(setq-default server-lock-file "~/.emacs.d/server-lock")
(defun start-server-if-not-running ()
  (if (not (file-exists-p server-lock-file))
      (progn (make-empty-file server-lock-file)
             (server-start)
             (add-hook 'kill-emacs-hook
                       (lambda () (delete-file server-lock-file))))))

(add-hook 'after-init-hook 'start-server-if-not-running)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'c++-mode-hook (lambda ()
                                (setq c-noise-macro-names '("constexpr"))
                                (add-to-list 'autocomplete/sources (lambda () (autocomplete/get-all-words-in-buffers-with-extensions ".cc" ".hh")))))
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))
(add-hook 'compilation-mode-hook 'visual-line-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'autocomplete/sources 'autocomplete/get-elisp-symbols)))

;; other
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq-default confirm-kill-emacs 'yes-or-no-p)
(setq-default custom-file "~/.emacs.d/custom.el")

(setq-default epa-pinentry-mode 'loopback)

;; dedicated windows
(defun mark-window-dedicated ()
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "Window marked dedicated"))

(defun mark-window-not-dedicated ()
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "Window unmarked dedicated"))

(defun mark-frame-unsplittable ()
  (interactive)
  (set-frame-parameter nil 'unsplittable t))

(defun mark-frame-splittable ()
  (interactive)
  (set-frame-parameter nil 'unsplittable nil))

(global-set-key "\C-c\ d" 'mark-window-dedicated)
(global-set-key "\C-c\ D" 'mark-window-not-dedicated)

(global-undo-tree-mode)

;; indentation
(defun newline-indent-match-previous ()
  (interactive)
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column col)))

(global-set-key "\t" 'tab-to-tab-stop)
(global-set-key (kbd "<backtab>") 'indent-according-to-mode)
(global-set-key (kbd "RET") 'newline-indent-match-previous)

(setq-default backward-delete-char-untabify-method nil)
(setq-default tab-width 8
              c-indent-offset 8
              c-basic-offset 8
              c-default-style '((other . "k&r"))
              indent-tabs-mode t)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(electric-indent-mode 0)

;; other keybinds
(global-set-key "\C-c\ c" 'compile)
(global-set-key "\C-c\ l" (lambda () (interactive) (insert-char ?\^L)))
(global-set-key "\C-c\ g" 'magit-status)
(global-set-key [?\e tab] 'centaur-tabs-forward)
(global-set-key [?\e backtab] 'centaur-tabs-backward)

;; utilities for configuring packages
(require 'cl)
(defun add-advice-once (trigger where function)
  (lexical-let
   ((lexical-trigger trigger)
    (lexical-function function))
   (advice-add trigger :after (lambda (&rest _rest) (advice-remove lexical-trigger lexical-function)))
   (advice-add trigger where function))
  nil)

(defmacro lazy-configure (trigger-function &rest forms)
  `(add-advice-once ,trigger-function :before (lambda (&rest _rest) ,@forms)))

(defun send-process (process-name string)
  (interactive "sprocess name: \nsinput: ")
  (process-send-string process-name (concat string "\n")))

;; ivy
(ivy-mode 1)
(setq-default ivy-use-virtual-buffers t
              enable-recursive-minibuffers t
              ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

;; autocompletion
(defun autocomplete/current-word ()
  (let ((end (point)))
    (save-excursion
      (while (progn
               (backward-char)
               (looking-at autocomplete/word-regexp)))
      (forward-char)
      (buffer-substring (point) end))))

(defun autocomplete/predicate (a b)
  (if (< (length a) (length b))
      t
    nil))

(defvar autocomplete/word-regexp "\\sw\\|\\s_")

(defun autocomplete/get-next-word (direction)
  (save-excursion
    (let ((start (point)))
      (if (= start (point-max))
        nil
        (while (looking-at autocomplete/word-regexp)
          (forward-char direction))
        (buffer-substring start (point))))))

(defun autocomplete/get-buffer-words-from-point (direction)
  (save-excursion
    (let ((next-word (autocomplete/get-next-word direction))
          (words nil))
      (while next-word
        (progn
          (setq words (cons next-word words))
          (forward-char (* direction (length next-word)))
          (while (and (not (looking-at autocomplete/word-regexp))
                      (not (= (point) (point-max))))
            (forward-char direction))
          (setq next-word (autocomplete/get-next-word direction))))
      words)))

(defun autocomplete/get-buffer-words ()
  (save-excursion
    (goto-char (point-min))
    (autocomplete/get-buffer-words-from-point 1)))

(defun autocomplete/get-elisp-symbols ()
  (let ((symbol-names nil))
    (mapatoms
     (lambda (symbol)
       (if (symbolp symbol)
           (setq symbol-names (cons (symbol-name symbol) symbol-names)))))
     symbol-names))

(defun autocomplete/get-all-words-in-buffers-with-extensions (&rest extensions)
  (let ((buffers
         (seq-filter (lambda (buffer)
                       (member t (mapcar (lambda (extension)
                                           (equal (substring (buffer-name buffer) (- (length extension))) extension))
                                         extensions)))
                       (buffer-list)))
        (words nil)
        (original-buffer (current-buffer)))
    (save-excursion
      (dolist (buffer buffers)
        (switch-to-buffer buffer t t)
        (setq words (append words (autocomplete/get-buffer-words))))
      (switch-to-buffer original-buffer t t))
      words))

(defvar autocomplete/sources
  '(autocomplete/get-buffer-words))
(make-variable-buffer-local 'autocomplete/sources)

(defun autocomplete/get-expansions ()
  (let ((words (make-hash-table :test 'equal)))
    (dolist (generator autocomplete/sources (hash-table-keys words))
      (dolist (word (funcall generator)) (puthash word t words)))))

(defun autocomplete ()
  (interactive)
  (let ((to-expand (autocomplete/current-word)))
    (let ((expansions (remove to-expand (autocomplete/get-expansions))))
      (if (= (length expansions) 0)
          (message "No completions")
        (let ((to-insert
               (ivy-completing-read
                "Expansion: "
                (sort expansions 'autocomplete/predicate)
                nil t to-expand nil nil nil)))
          (kill-region (- (point) (length to-expand)) (point))
          (insert to-insert))))))

(global-set-key "\C-t" 'autocomplete)

;; erc
(defun erc-autoconnect ()
  (interactive)
  (erc :server "irc.freenode.net")
  (erc :server "irc.rizon.net"))

(lazy-configure
 'erc
 (setq-default frame-title-format "IRC")
 (add-to-list 'erc-modules 'notifications)
 (erc-services-mode 1)
 (erc-update-modules)
 (setq-default erc-nick "zovt"
               erc-nick-uniquifier "_"
               erc-away-nickname "zovt [away]"
               erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#gentoo" "#loam")
                                             ("rizon.net" "#rice")
                                             ("oftc.net" "#llvm"))
               erc-autojoin-timing 'ident
               erc-prompt-for-nickserv-password nil
               erc-kill-buffer-on-part t
               erc-kill-queries-on-quit t
               erc-kill-server-buffer-on-quit t
               erc-autoaway-idle-seconds 600))

;; mu4e
(lazy-configure
 'mu4e
 (setq-default frame-title-format "Mail")
 (setq-default mu4e-sent-folder "/sent"
               mu4e-drafts-folder "/drafts"
               mu4e-maildir "~/.mail"
               mu4e-bookmarks `(("flag:unread AND NOT flag:trashed" "Unread messages" 117)
                                ("date:today..now" "Today's messages" 116)
                                ("date:7d..now" "Last 7 days" 119)
                                ("mime:image/*" "Messages with images" 112)
                                ,(make-mu4e-bookmark :name "All messages" :query "" :key ?a))
               user-mail-address "nickippoliti@posteo.net"
               user-full-name "Nick Ippoliti"
               message-send-mail-function 'smtpmail-send-it
               smtpmail-smtp-user "nickippoliti@posteo.net"
               smtpmail-smtp-server "posteo.de"
               smtpmail-smtp-service 587)
 (defvar my/mail-account-alist
   '(("Posteo"
      (mu4e-sent-folder "/Posteo/Sent")
      (mu4e-drafts-folder "/Posteo/Drafts")
      (user-mail-address "nickippoliti@posteo.net")
      (user-full-name "Nick Ippoliti")
      (smtpmail-smtp-user "nickippoliti@posteo.net")
      (smtpmail-smtp-server "posteo.de")
      (smtpmail-smtp-service 587))
     ("School"
      (mu4e-sent-folder "/School/[Gmail].Sent Mail")
      (mu4e-drafts-folder "/School/[Gmail].Drafts")
      (user-mail-address "ippoliti.n@husky.neu.edu")
      (user-full-name "Nick Ippoliti")
      (smtpmail-smtp-user "ippoliti.n@husky.neu.edu")
      (smtpmail-smtp-server "smtp.gmail.com")
      (smtpmail-smtp-service 587))
      ))
 (defun my/mu4e-set-account ()
   "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
   (let* ((account
           (if mu4e-compose-parent-message
               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                 (string-match "/\\(.*?\\)/" maildir)
                 (match-string 1 maildir))
             (completing-read (format "Compose with account: (%s) "
                                      (mapconcat #'(lambda (var) (car var))
                                                 my/mail-account-alist "/"))
                              (mapcar #'(lambda (var) (car var)) my/mail-account-alist)
                              nil t nil nil (caar my/mail-account-alist))))
          (account-vars (cdr (assoc account my/mail-account-alist))))
     (if account-vars
         (mapc #'(lambda (var)
                   (set (car var) (cadr var)))
               account-vars)
       (error "No email account found"))))
 (add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-account)

 (setq-default offlineimap-lock-file "~/.emacs.d/offlineimap-lock")
 (defun start-offlineimap-if-needed (&rest _rest)
   (unless (file-exists-p offlineimap-lock-file)
     (make-empty-file offlineimap-lock-file)
     (unless (get-buffer "*<offlineimap process>*")
       (make-process :name "offlineimap" :buffer "*<offlineimap process>*"
                     :command '("offlineimap")))
     (add-hook 'kill-emacs-hook
                       (lambda ()
                         (delete-file offlineimap-lock-file)
                         (if (get-process "offlineimap")
                             (delete-process "offlineimap"))))))
 (advice-add 'mu4e :before 'start-offlineimap-if-needed)
 (start-offlineimap-if-needed)

 (defun offline-imap-signin (account-name)
   (interactive "saccount name: ")
   (let ((account-file (concat "~/.emacs.d/mail/" account-name ".gpg")))
     (send-process "offlineimap" (read-file-to-string account-file))))

 (defun offline-imap-refresh ()
   (interactive)
   (signal-process "offlineimap" 'SIGUSR1))

 (mu4e-alert-set-default-style 'libnotify)
 (mu4e-alert-enable-notifications)
 (run-with-timer 60 60 'mu4e-update-index))

;; elfeed
(defun get-youtube-feed-url (page)
  (interactive "syoutube channel url: ")
  (let ((content-buffer (url-retrieve-synchronously page))
        (original-buffer (current-buffer)))
    (switch-to-buffer content-buffer t t)
    (search-forward "channel-external-id")
    (forward-char 2)
    (zap-up-to-char 1 ?\")
    (switch-to-buffer original-buffer t t)
    (insert "\"https://www.youtube.com/feeds/videos.xml?channel_id=")
    (yank)
    (insert "\"")))

(lazy-configure
 'elfeed
 (setq-default frame-title-format "Elfeed")

 (defun play-youtube-link (beginning end)
   (interactive "r")
   (start-process "youtube" "*mpv*" "mpv" (buffer-substring-no-properties beginning end))
   (message "Playing youtube link in mpv"))

 (setq-default elfeed-feeds
               '("https://lukesmith.xyz/rss.xml"
                 "https://danluu.com/atom.xml"
                 "https://eli.thegreenplace.net/feeds/all.atom.xml"
                 "https://nullprogram.com/index/"
                 "http://insanecoding.blogspot.com/feeds/posts/default"
                 "http://feeds.hanselman.com/ScottHanselman"
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" ; Luke Smith
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg" ; Wolfgang's Channel
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCLsiaNUb42gRAP7ewbJ0ecQ" ; The Command Zone
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCPqT2ULat4WIzWKqpAAOlIQ" ; Jumbo Commander
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCG8Yi6I_XYjYtvFgOleqYxg" ; MTG Muddstah
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCN0-RRaxMgh86eOwndAklxw" ; The Golden One
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCqsMrAu-93PJhjR9NxtW30w" ; Elessar
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCaHx0T1LWrVKWF1XfWWLSuw" ; Omar Isuf
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCKf0UqBiCQI4Ol0To9V0pKQ" ; Buff Dudes
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCC3L8QaxqEGUiBC252GHy3w" ; Molyneux
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCKTehwyGCKF-b2wo0RKwrcg" ; Bisqwit
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCCuoqzrsHlwv1YyPKLuMDUQ" ; JBlow
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCepFoY2QjSR5SlMnKGlzJtg" ; Rargh
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCEFPJ3Qc_0tvKQBYDO-UKvQ" ; Link
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCSFaYYQzNMLo2U6rSNLpghg" ; Torbjorn
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCxPWgo_ujtjKT6vn1-Obm2g" ; The world tree
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCZAENaOaceQUMd84GDc26EA" ; Survive the Jive
                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCcaVClI50rGZmbYMhoSSDGA" ; Northmen
                 ))
 )


(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))

(speedup-defer 0.1 (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
(put 'erase-buffer 'disabled nil)
