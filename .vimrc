set guifont=Iosevka\ Term\ Medium\:h17
syntax off

set t_Co=256
set bg=dark

set guicursor+=a:blinkon0
set guioptions-=T
set guioptions-=r
set guioptions-=L

highlight clear
highlight Normal guibg=#1C1C1C guifg=#FFFFFF
highlight EndOfBuffer guifg=#1C1C1C
highlight VertSplit guibg=#1C1C1C guifg=#1C1C1C
highlight StatusLine guibg=#1C1C1C guifg=#1C1C1C gui=NONE
highlight StatusLineNC guibg=#1C1C1C guifg=#1C1C1C gui=NONE

autocmd!
autocmd BufWritePre * :%s/\s\+$//e

inoremap <Esc> <Esc>:

nnoremap <MiddleMouse> <Nop>
nnoremap <2-MiddleMouse> <Nop>
nnoremap <3-MiddleMouse> <Nop>
nnoremap <4-MiddleMouse> <Nop>

inoremap <MiddleMouse> <Esc>"+pa
inoremap <2-MiddleMouse> <Esc>"+pa
inoremap <3-MiddleMouse> <Esc>"+pa
inoremap <4-MiddleMouse> <Esc>"+pa

vnoremap <C-x> "+ygvxi
vnoremap <BS> xi
vmap <MiddleMouse> <C-x>

nmap <LeftRelease> i

set ts=2
set shiftwidth=2
set noexpandtab
set smartindent
set mouse=a

set bs=2

