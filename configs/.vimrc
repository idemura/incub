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
set tabstop=2
set shiftwidth=2
set expandtab
set cc=80
set incsearch
set number
set hidden
set nowrap
set noswapfile
set wildignore+=*.o,*.a,*.o.d,*_test
" Set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim

" Clear filetype flags before changing runtimepath to force Vim to reload
" them.
filetype off
filetype plugin indent off
set runtimepath+=$GOROOT/misc/vim
filetype plugin indent on

" Do not expand tab to spaces in make files.
autocmd FileType make setlocal noexpandtab
autocmd BufWritePre * TrailerTrim

let mapleader=" "
nnoremap <leader>o :CtrlP<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>s :sh<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

call vundle#begin()
" Alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" Let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Bundle 'kien/ctrlp.vim'
Bundle 'Valloric/YouCompleteMe'
Bundle 'scrooloose/nerdcommenter'
Bundle 'csexton/trailertrash.vim'
" Bundle 'jiangmiao/auto-pairs'
" Bundle 'fatih/vim-go'

" All of your Plugins must be added before the following line
call vundle#end()

