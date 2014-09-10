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
