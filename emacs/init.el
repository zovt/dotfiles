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
(install-if-needed 'esup)
(install-if-needed 'slack)

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

(set-face-font 'default "Iosevka Term Slab Light 16")
(set-face-font 'variable-pitch "Liberation Serif 17")
(set-fontset-font t 'unicode "EmojiOne" nil 'prepend)
(setq-default backup-by-copying t
              delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2)

(deftheme zovt "My colors")
(let ((class '((class color) (min-colors 256)))
      (default-face '(:foreground "black" :background "white")))
  (custom-theme-set-faces
   'zovt
   `(default                        ((,class ,default-face)))
   `(mode-line                      ((,class (:foreground "white" :background "black" :box "black"))))
   `(mode-line-inactive             ((,class (:foreground "black" :background "white" :box "black"))))
   `(fringe                         ((,class (:background "white"))))
   `(font-lock-doc-face             ((,class ,default-face)))
   `(font-lock-type-face            ((,class ,default-face)))
   `(font-lock-builtin-face         ((,class ,default-face)))
   `(font-lock-function-name-face   ((,class ,default-face)))
   `(font-lock-string-face          ((,class ,default-face)))
   `(font-lock-variable-name-face   ((,class ,default-face)))
   `(font-lock-constant-face        ((,class ,default-face)))
   `(font-lock-comment-face         ((,class (:foreground "black" :background "white" :slant italic))))
   `(region                         ((,class (:foreground "black" :background "gray89"))))
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
                                (setq c-noise-macro-names '("constexpr"))))
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))

;; other
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
(global-set-key "\C-t" 'hippie-expand)
(global-set-key "\C-c\ c" 'compile)
(global-set-key "\C-c\ l" (lambda () (interactive) (insert-char ?\^L)))
(global-set-key "\C-c\ g" 'magit-status)

;; utilities for configuring non-text-editor packages
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

;; erc
(defun erc-autoconnect ()
  (interactive)
  (erc :server "irc.freenode.net")
  (erc :server "irc.rizon.net"))

(lazy-configure
 'erc
 (add-to-list 'erc-modules 'notifications)
 (erc-services-mode 1)
 (erc-update-modules)
 (setq-default erc-nick "zovt"
               erc-nick-uniquifier "_"
               erc-away-nickname "zovt [away]"
               erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#gentoo")
                                             ("rizon.net" "#rice")
                                             ("oftc.net" "#llvm"))
               erc-autojoin-timing 'ident
               erc-prompt-for-nickserv-password nil
               erc-kill-buffer-on-part t
               erc-kill-queries-on-quit t
               erc-kill-server-buffer-on-quit t
               erc-autoaway-idle-seconds 600))

;; slack
(lazy-configure
 'slack-start
 (setq-default slack-prefer-current-team t
               slack-buffer-emojify t)
 (slack-register-team
  :name "FriendGroup"
  :default t
  :client-id "110c5ae1-1562126312.440"
  :token "xoxs-112142280547-159670934352-676202934097-ed94b78db642c13de0d3b374799b8b292946bb220d09a917c43f534b8cbd5e0e"
  :subscribed-channels '(programming mtg)
  :full-and-display-names t))

;; mu4e
(lazy-configure
 'mu4e
 (setq-default mu4e-maildir "~/.mail"
               mu4e-sent-folder "/Posteo/Sent"
               mu4e-drafts-folder "/Posteo/Drafts"
               user-mail-address "nickippoliti@posteo.net"
               user-full-name "Nick Ippoliti"
               message-send-mail-function 'smtpmail-send-it
               smtpmail-smtp-user "nickippoliti@posteo.net"
               smtpmail-smtp-server "posteo.de"
               smtpmail-smtp-service 587)
 (defvar my/mu4e-account-alist
   '(("Posteo"
      (mu4e-sent-folder "/Posteo/Sent")
      (user-mail-address "nickippoliti@posteo.net")
      (user-full-name "Nick Ippoliti")
      (smtpmail-smtp-user "nickippoliti@posteo.net")
      (smtpmail-smtp-server "posteo.de")
      (smtpmail-smtp-service 587))
     ("School"
      (mu4e-sent-folder "/School/[Gmail].Sent Mail")
      (user-mail-address "ippoliti.n@husky.neu.edu")
      (user-full-name "Nick Ippoliti")
      (smtpmail-smtp-user "ippoliti.n@husky.neu.edu")
      (smtpmail-smtp-server "smtp.gmail.com")
      (smtpmail-smtp-service 587))
      ))

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
                                                 my/mu4e-account-alist "/"))
                              (mapcar #'(lambda (var) (car var)) my/mu4e-account-alist)
                              nil t nil nil (caar my/mu4e-account-alist))))
          (account-vars (cdr (assoc account my/mu4e-account-alist))))
     (if account-vars
         (mapc #'(lambda (var)
                   (set (car var) (cadr var)))
               account-vars)
       (error "No email account found"))))
 (add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-account))


(if (file-exists-p "~/.emacs.d/local.el") (load-file "~/.emacs.d/local.el"))

(speedup-defer 0.1 (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
