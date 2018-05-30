vc()
{
		if [ -d ".git" ]; then git $@;
		elif [ -d ".hg" ]; then hg $@;
		elif [ -d ".pijul" ]; then pijul $@;
		else echo "VC directory not found!"
		fi
}
