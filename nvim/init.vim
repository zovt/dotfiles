" vim: set foldmethod=marker foldlevel=0:
" basics {{{
" default

" change tab width
set tabstop=2
set shiftwidth=2

" show line numbers
set number
set relativenumber

" show most recent command in bottom bar
set showcmd

" indent based on filetype
filetype indent on

" set wildmenu
set wildmenu

" make at least 1 line visible when scrolling
set scrolloff=5

" only redraw when needed
set lazyredraw

" show matching parens
set showmatch

" incremental searching with highlights
set incsearch
set hlsearch

" enable modeline
set modelines=1

" turn on folding
set foldenable
set foldmethod=indent

" show all folds by default
set foldlevelstart=10

" set maximum fold number
set foldnestmax=10

" visual bell
set visualbell

" make backspace good
set backspace=indent,eol,start

" change backups
set backupdir=~/vimfiles/backups//
set directory=~/vimfiles/backups//,.

" keep files open even when to shown on screen
set hidden

" enable mouse
set mouse=a
" }}}
" plugins {{{
if has("win32") 
	set runtimepath^=~/AppData/Local/nvim/files
	call plug#begin('~/AppData/Local/nvim/files')
else
	set runtimepath^=~/.config/nvim/files/
	call plug#begin('~/.config/nvim/files/plugged/')
endif

Plug 'zovt/simple-colorschemes'
Plug 'NLKNguyen/papercolor-theme'
Plug 'dracula/vim'
Plug 'arcticicestudio/nord-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'neomake/neomake'
Plug 'lervag/vimtex'
Plug 'dpc/vim-smarttabs'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'ap/vim-buftabline'
Plug 'fatih/vim-go'
Plug 'zchee/deoplete-go'
Plug 'w0rp/ale'

call plug#end()

" Plugin Options {{{
" NeoMake
let g:neomake_cpp_clangcheck_args = ["-std=c++14", "-Wextra", "-Wall"]
let g:neomake_cpp_clang_args = ["-std=c++14", "-Wextra", "-Wall"]
autocmd! BufWritePost * Neomake

" Deoplete
let g:deoplete#enable_at_startup = 1

" Goyo
function! s:goyo_enter()
	set number
	set relativenumber
	Limelight
endfunction

function! s:goyo_leave()
	set number
	set relativenumber
	Limelight!
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" vim-go
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']

" lightline
let g:lightline = {
			\ 'colorscheme': 'nord',
			\ }

set laststatus=2

" }}}
" }}}
" Visual {{{
set guicursor+=a:blinkon0
set guioptions-=T
set guioptions-=m
set guioptions-=r
set guioptions-=L
set ruler

set t_Co=256
set background=dark
colorscheme nord

" syntax highlighting
syntax enable
syntax on

" set colorcolumn
set colorcolumn=80

" soft wrapping
set wrap
set linebreak
set nolist

" statusline
set statusline=%y\ %P\ %f\ %m

" show tabline
set showtabline=2
" }}}
" Keybinds {{{
" leader
let mapleader="\<Space>"
nmap <space> <nop>

" leader space open/closes folds
nnoremap <TAB> za
nnoremap <S-Tab> zMzv

" turn off highlights
nnoremap <leader>z :nohlsearch<CR>

" move vertically by visual line
nnoremap j gj
nnoremap k gk

" move to end of line with CTRL-E in insert mode, and the front with CTRL-A
inoremap <C-a> <esc>0i
inoremap <C-e> <esc>$a

" map jk to esc
inoremap jk <esc>

" make ; :
nnoremap ; :

" load vimrc
nnoremap <leader>lv :source ~/_vimrc<CR>

" edit vimrc
nnoremap <leader>ev :e ~/_vimrc<CR>

" move between windows
nnoremap <leader>wh <C-w>h
nnoremap <leader>wl <C-w>l
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wq <C-w>q

" open new splits
nnoremap <leader>wH <C-W>S
nnoremap <leader>wV <C-W>v

" change to current directory
nnoremap <leader>cd :lcd %:p:h<CR>

" enter to autocomplete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" FZF
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fg :GFiles<CR>
nnoremap <leader>fm :Marks<CR>
nnoremap <leader>fa :Ag<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader><leader> :BLines<CR>

" }}}
" Windows {{{
if has("win32") 
	set runtimepath^=C:/Program\ Files/Neovim/share/nvim-qt/runtime
	let g:python3_host_prog = 'C:\Python35\python.exe'
	let g:python2_host_prog = 'C:\Python27\python.exe'
	source C:\Users\nicho\AppData\Local\nvim\ginit.vim
	set backupdir=~/AppData/Local/nvim/files/backups//
	set directory=~/AppData/Local/nvim/files/backups//,.
endif
" }}}
