(setq gnus-secondary-select-methods
      '((nnimap "first-name" ; primary email
		(nnimap-address "first@email")
		(nnimap-server-port 993)
		(nnimap-authenticator login)
		(nnimap-expunge-on-close 'never)
		(nnimap-stream ssl))
	(nnimap "second-name" ;; secondary email
		(nnimap-address "second@email")
		(nnimap-server-port 993)
		(nnimap-authenticator login)
		(nnimap-expunge-on-close 'never)
		(nnimap-stream ssl))))

(if (eq system-type 'windows-nt) (progn 
				   (setq tls-program '("C:/Program Files (x86)/Git/bin/openssl.exe s_client -connect %h:%p -no_ssl2 -ign_eof"))))
(setq user-mail-address "email")
(setq user-full-name "name")
