vc()
{
		if [ -d ".git" ]; then git $@;
		elif [ -d ".hg" ]; then hg $@;
		elif [ -d ".pijul" ]; then pijul $@;
		else echo "VC directory not found!"
		fi
}

export EDITOR=emacsclient

export PS1='$(local WD=${PWD/$HOME/\~}; echo $WD | sed "s,\([^A-z]\)\([A-z]\)[A-z]*,\1\2,g") ; '
