vc_command()
{
		if [ -d ".git" ]; then git $@;
		elif [ -d ".hg" ]; then hg $@;
		elif [ -d ".pijul" ]; then pijul $@;
		else echo "VC directory not found!"
		fi
}
alias uh='vc_command'

mount_media()
{
		if [ -d "~/m/files" ]; then
				sshfs -o Compression=no zovt@sputnik.whatbox.ca: ~/m;
				echo "Media mounted"
		else
				echo "Media already mounted"
		fi
}
alias mm='mount_media'

alias mu='ncmpcpp'

export EDITOR=emacsclient

export PS1='$(local WD=${PWD/$HOME/\~}; echo $WD | sed -r "s,([^a-zA-Z])([a-zA-Z])[a-zA-Z]*,\1\2,g") ; '
