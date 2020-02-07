set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set nojoinspaces

" fd returns to normal mode
inoremap fd <esc>

"
" Plug {
"
call plug#begin('~/.vim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'altercation/vim-colors-solarized'
Plug 'kien/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()
" }

"
" Plugin configuration {
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

let g:syntastic_python_checkers = ['flake8']
let g:syntastic_javascript_checkers = ['eslint']
" }

let g:pymode_python = 'python3'

" }

source $HOME/.local/vimrc
