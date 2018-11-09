require('vis')
require('filetype')
require('complete-word')

vis.events.subscribe(vis.events.INIT, function()
	vis:command('set tabwidth 2')
	vis:command('set autoindent on')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	local lexers = vis.lexers
	lexers.STYLE_DEFAULT = 'fore:default,back:default'
	lexers.STYLE_CURSOR = 'reverse'
	lexers.STYLE_STATUS = 'reverse'
	lexers.STYLE_STATUS_FOCUS = 'reverse,bold'
	lexers.STYLE_INFO = 'reverse'
	lexers.STYLE_SELECTION = 'fore:#ffffff,back:#ffaaaa'
	lexers.STYLE_LINENUMBER = 'fore:black'
	lexers.STYLE_LINENUMBER_CURSOR = 'fore:#88bbaa'
	vis:command('set syntax off')
	vis:command('set number')
end)
