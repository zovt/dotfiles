# git
function gl
	git log --pretty=oneline
end

function gc
	git commit -am $argv
end

function gp
	git push $argv
end

function gs
	git status $argv
end

function ga
	git add -A
end

function gaa
	git add -A
end

function gb
	git branch $argv
end

function gm
	git merge $argv
end
