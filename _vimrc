execute pathogen#infect()


" Indent automatically depending on filetype
filetype indent on
set autoindent

" Turn on line numbering. Turn it off with "set nonu" 
set number

" Set syntax on
syntax on

" Case insensitive search
set ic

" Higlhight search
set hls

" Wrap text instead of being on one line
set lbr

" Change colorscheme from default to delek
colorscheme wombat 

" Font options
set encoding=utf-8
set guifont:Sauce_Code_Powerline:h10:cANSI

" Set up airline fancy fonts 
let g:airline_powerline_fonts = 1

" Set up folds
set foldmethod=marker

set guioptions-=m
set guioptions-=T
set guioptions-=r

set nocompatible ruler laststatus=2 showcmd showmode
set incsearch ignorecase smartcase hlsearch
set shortmess+=I

" Custom binds
map <F9> :e $HOME/_vimrc<CR>
map <F6> :so $HOME/_vimrc<CR>
map <C-n> :NERDTreeToggle<CR>
map <C-s> :w<CR>
set langmenu=en_US.UTF-8
let $LANG = 'en'




