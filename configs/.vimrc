filetype off
filetype indent on
filetype plugin on

syntax on

set nocompatible
set autoread
set autoindent
" Case insensitive search
set ic
" Highlight search
" set hlsearch
set tabstop=4
set shiftwidth=4
set expandtab
set cc=81
set incsearch
set number
set hidden
set nowrap
set gdefault
set noswapfile
set wildignore+=*.o,*.a,*.o.d,*_test,*.pyc

" Do not expand tab to spaces in make files.
autocmd FileType make setlocal noexpandtab

nnoremap <CR> :
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

let mapleader=" "

nnoremap <leader>w        :w<CR>
nnoremap <leader>q        :q<CR>
nnoremap <leader>s        :sh<CR>
nnoremap <leader>l        :ls<CR>
nnoremap <leader><Left>   :bn<CR>
nnoremap <leader><Right>  :bp<CR>

nnoremap <leader>K diw
nnoremap <leader>Q di"
