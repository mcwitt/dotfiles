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
Plugin 'tpope/vim-fugitive'
Plugin 'altercation/vim-colors-solarized'
Plugin 'aperezdc/vim-template'
Plugin 'nathanaelkane/vim-indent-guides'

Plugin 'vim-scripts/dbext.vim'

Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'davidhalter/jedi-vim'

Plugin 'pangloss/vim-javascript'
Plugin 'elzr/vim-json'

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

" LaTeX-Box {
let g:tex_flavor='latex'    " assume .tex files are LaTeX
let g:LatexBox_viewer='xpdf'
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_quickfix=2   " prevent quickfix window from stealing cursor
" }
" }

source $HOME/.local/vimrc
