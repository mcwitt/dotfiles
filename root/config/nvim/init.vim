set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set nojoinspaces

" fd returns to normal mode
inoremap fd <esc>

"
" vim-plug {
"
call plug#begin('~/.local/share/nvim/site/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'altercation/vim-colors-solarized'
Plug 'davidhalter/jedi-vim'
Plug 'kien/ctrlp.vim'
Plug 'klen/python-mode'
Plug 'lambdalisue/vim-pyenv'
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'valloric/youcompleteme'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()
" }

"
" plugin configuration {
"
" solarized {
set background=dark
colorscheme solarized
" }

" syntastic {
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

