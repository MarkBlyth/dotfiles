set nocompatible
filetype off

set shell=/bin/bash

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" Add plugins here"


Plugin 'terryma/vim-multiple-cursors' " Multiple cursors
" Plugin 'Valloric/YouCompleteMe' " Autocomplete
Plugin 'zxqfl/tabnine-vim' " Autocomplete

" /plugins "
call vundle#end()
filetype plugin indent on

set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set number
set et
set smartindent
set showmatch
syntax on
