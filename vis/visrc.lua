require('vis')

vis.events.subscribe(vis.events.INIT, function()
	vis:command('set autoindent')
	vis:command('set tabwidth 2')

	vis:command('map! normal ; :')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	vis:command('set number')
	vis:command('set theme bw')
	vis:command('set colorcolumn 80')
	vis:command('set syntax off')
end)
