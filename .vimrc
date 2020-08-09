syntax on

set guicursor=
"set guicursor+=n-v-c:blinkon0
set relativenumber
set nohlsearch
set noerrorbells
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set incsearch
set clipboard=unnamed
set cmdheight=1
set colorcolumn=88
highlight ColorColumn ctermbg=0 guibg=lightgrey

filetype plugin on
filetype indent plugin on
au! FileType python setl nosmartindent

autocmd BufWritePre * :%s/\s\+$//e

call plug#begin('~/.vim/plugged')
" call plug#begin('~/.config/nvim/plugged')
Plug 'morhetz/gruvbox'
Plug 'franbach/miramare'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'
Plug 'tpope/vim-fugitive'
Plug 'jremmen/vim-ripgrep'
Plug 'junegunn/fzf.vim'
Plug 'jreybert/vimagit'
Plug 'preservim/nerdcommenter'
Plug 'vim-utils/vim-man'
Plug 'mbbill/undotree'
Plug 'kien/ctrlp.vim'
Plug 'machakann/vim-highlightedyank'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug '/home/mpaulson/personal/vim-be-good'

Plug 'nvie/vim-flake8'
Plug 'vim-scripts/indentpython.vim'
Plug 'fisadev/vim-isort'

Plug 'preservim/nerdtree'
Plug 'ycm-core/YouCompleteMe'

call plug#end()

set t_Co=256
let g:gruvbox_contrast_dark = 'medium'
"let g:dracula_colorterm = 0

" --- The Greatest plugin of all time.  I am not bias
let g:vim_be_good_floating = 1


set background=dark

colorscheme gruvbox

"let g:airline_theme='twofirewatch'

" Call flake8 on save buffer
autocmd BufWritePost *.py call flake8#Flake8()
autocmd FileType python cnoreabbrev <expr> q winnr("$") > 1 && getcmdtype() == ":" && getcmdline() == 'q' ? 'ccl <BAR> q' : 'q'

" Configure NerdTree
map <C-n> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:NERDCommentEmptyLines = 1

" Split line with Ctrl J
nnoremap <NL> i<CR><ESC>

let mapleader = ","

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

let g:airline#extensions#branch#enabled = 0

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
"let g:airline_left_sep = '»'
"let g:airline_left_sep = '▶'
"let g:airline_right_sep = '«'
"let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = '☰'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
"let g:airline_left_sep = ''
"let g:airline_left_alt_sep = ''
"let g:airline_right_sep = ''
"let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
