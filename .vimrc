set guifont=Iosevka\ Term\ Medium\ 15
syntax off

set t_Co=256
set bg=light

set guicursor+=a:blinkon0
set guioptions-=T
set guioptions-=r
set guioptions-=L
set foldcolumn=1

highlight clear
highlight Normal guibg=#FFFFEA guifg=#000000
highlight EndOfBuffer guifg=#FFFFEA
highlight VertSplit guibg=#99994C guifg=#99994C
highlight StatusLine guibg=#EAFFFF guifg=#000000 gui=NONE
highlight StatusLineNC guibg=#BAFFFF guifg=#000000 gui=NONE
highlight Search guibg=#EEEE9E
highlight Visual guibg=#CAFFFF
highlight FoldColumn guibg=#FFFFEA guifg=#FFFFEA

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
set incsearch
set hlsearch
