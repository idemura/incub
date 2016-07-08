set nocompatible
filetype off
filetype indent on
set autoread
set autoindent
syntax on
" Case insensitive search
set ic
" Highlight search
" set hls
set tabstop=2
set shiftwidth=2
set expandtab
set cc=80
set incsearch
set nu
set hidden
set nowrap
set noswapfile
" Do not expand tab to spaces in make files.
autocmd FileType make setlocal noexpandtab

let mapleader=" "
nnoremap <leader>o :CtrlP<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>s :sh<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

command TT TrailerTrim

" filetype plugin on

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

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

" All of your Plugins must be added before the following line
call vundle#end()
