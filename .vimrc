set nocompatible
filetype off

set shell=/bin/bash

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" Add plugins here"


Plugin 'terryma/vim-multiple-cursors' " Multiple cursors
Plugin 'Valloric/YouCompleteMe' " Autocomplete


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

" Bits to get lightline to look nicer
" set laststatus=2
" if !has('gui_running')
"    set t_co=256
" end

" Stuff for syntastic
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 0
"let g:syntastic_check_on_wq = 0
"let g:syntastic_check_on_w = 0
