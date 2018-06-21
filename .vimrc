set guifont=Iosevka\ Term\ Medium\ 15
syntax off

set t_Co=256
set bg=dark

set guicursor+=a:blinkon0
set guioptions-=T
set guioptions-=r
set guioptions-=L

highligh clear
highlight Normal guibg=#1C1C1C guifg=#FFFFFF
highlight EndOfBuffer guifg=#1C1C1C
highlight VertSplit guibg=#1C1C1C guifg=#1C1C1C
highlight StatusLine guibg=#1C1C1C guifg=#1C1C1C gui=NONE
highlight StatusLineNC guibg=#1C1C1C guifg=#1C1C1C gui=NONE

autocmd!
autocmd BufWritePre * :%s/\s\+$//e

imap <M-x> <Esc>:
imap <C-s> <Esc>/
imap <C-v> <Esc>pli

vmap <C-c> yi
vmap <C-x> di
vmap <BS> "_di
