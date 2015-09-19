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

filetype plugin on

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

colorscheme delek

call vundle#begin()
" Alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" Let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Bundle 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}
Bundle 'kien/ctrlp.vim'
Bundle 'Valloric/YouCompleteMe'
" Bundle 'tpope/vim-commentary'
Bundle 'scrooloose/nerdcommenter'
Bundle 'jiangmiao/auto-pairs'

" All of your Plugins must be added before the following line
call vundle#end()

" noremap i k
" noremap k j
" noremap j h
" noremap h i

