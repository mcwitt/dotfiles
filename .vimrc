set tabstop=4
set softtabstop=4
set shiftwidth=4

set expandtab
set nojoinspaces

syntax enable

" hide scroll bar when GUI running
if has('gui_running')
    set guioptions-=r  "scrollbar
endif

"
" VUNDLE {
"
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vim-scripts/dbext.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'scrooloose/syntastic'
Plugin 'altercation/vim-colors-solarized'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'tpope/vim-fugitive'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'pangloss/vim-javascript'
Plugin 'elzr/vim-json'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-surround'
Plugin 'aperezdc/vim-template'

call vundle#end()            " required
filetype plugin indent on    " required
" }

"
" PLUGIN CONFIGURATION {
"
" Solarized {
set background=dark
colorscheme solarized
" }

" Syntastic {
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_javascript_checkers = ['eslint']
" }

" LaTeX-Box {
let g:tex_flavor='latex'    " assume .tex files are LaTeX
let g:LatexBox_viewer='xpdf'
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_quickfix=2   " prevent quickfix window from stealing cursor
" }


" }

source $HOME/.local/vimrc
