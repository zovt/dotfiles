export PS1='$(local WD=${PWD/$HOME/\~}; echo $WD | sed -E "s,([^a-zA-Z])([a-zA-Z])[a-zA-Z]*,\1\2,g") ; '
