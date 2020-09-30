set guicursor=
set relativenumber
"set cursorline
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
set cmdheight=2
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

filetype on
filetype plugin indent on
syntax on

au! FileType python setl nosmartindent

autocmd BufWritePre * :%s/\s\+$//e
autocmd InsertEnter,InsertLeave * set cul!

"call plug#begin('~/.vim/plugged')
call plug#begin('~/.config/nvim/plugged')

Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'
Plug 'tpope/vim-fugitive'
Plug 'jremmen/vim-ripgrep'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'jreybert/vimagit'
Plug 'majutsushi/tagbar'
Plug 'preservim/nerdcommenter'
Plug 'vim-utils/vim-man'
Plug 'mbbill/undotree'
Plug 'machakann/vim-highlightedyank'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'kaicataldo/material.vim', { 'branch': 'main' }
"Plug 'ap/vim-buftabline'
Plug 'airblade/vim-gitgutter'
Plug 'lepture/vim-jinja'
Plug 'pangloss/vim-javascript'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug '/home/mpaulson/personal/vim-be-good'
Plug 'nvie/vim-flake8'
Plug 'vim-scripts/indentpython.vim'
Plug 'psf/black', { 'branch': 'stable' }
Plug 'fisadev/vim-isort'
Plug 'preservim/nerdtree'
Plug 'ycm-core/YouCompleteMe'
Plug 'mattn/emmet-vim'
Plug 'sheerun/vim-polyglot'

call plug#end()

"if !has('nvim')
  "set ttymouse=xterm2
"endif

" always show the status bar
set laststatus=2

" sane text files
set fileformat=unix
set encoding=utf-8
set fileencoding=utf-8

" --- The Greatest plugin of all time.  I am not bias
let g:vim_be_good_floating = 1

if (has("nvim"))
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
if (has("termguicolors"))
  set termguicolors
endif
"let g:material_theme_style = 'palenight'
"let g:onedark_termcolors=256
set background=dark
colorscheme palenight

if executable('rg')
    let g:rg_derive_root = 'true'
endif

let g:ycm_python_binary_path = '/usr/bin/python3'

let mapleader = ","
let g:netrw_browse_split = 2
let g:netrw_banner = 0
let g:netrw_winsize = 25
let g:palenight_terminal_italics=1
let g:ctrlp_use_caching = 0

nmap <leader>h :wincmd h<CR>
nmap <leader>j :wincmd j<CR>
nmap <leader>k :wincmd k<CR>
nmap <leader>l :wincmd l<CR>
nmap <leader>u :UndotreeShow<CR>
nmap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nmap <Leader>rg :Rg<SPACE>
nmap <Leader>+ :vertical resize +5<CR>
nmap <Leader>- :vertical resize -5<CR>

" Split window
"nmap ss :split<Return><C-w>w
"nmap sv :vsplit<Return><C-w>w

" move through split windows
nmap <leader><Up> :wincmd k<CR>
nmap <leader><Down> :wincmd j<CR>
nmap <leader><Left> :wincmd h<CR>
nmap <leader><Right> :wincmd l<CR>

" move through buffers
nmap <leader>[ :bp!<CR>
nmap <leader>] :bn!<CR>
nmap <leader>x :bd<CR>

" word movement
imap <S-Left> <Esc>bi
nmap <S-Left> b
imap <S-Right> <Esc><Right>wi
nmap <S-Right> w

" indent/unindent with tab/shift-tab
nmap <Tab> >>
imap <S-Tab> <Esc><<i
nmap <S-tab> <<

" mouse
set mouse=a
let g:is_mouse_enabled = 1
"function ToggleMouse()
    "if g:is_mouse_enabled == 1
        "echo "Mouse OFF"
        "set mouse=
        "let g:is_mouse_enabled = 0
    "else
        "echo "Mouse ON"
        "set mouse=a
        "let g:is_mouse_enabled = 1
    "endif
"endfunction
"noremap <silent> <Leader>m :call ToggleMouse()<CR>
" code folding
set foldmethod=indent
set foldlevel=99

" tag list
map <leader>t :TagbarToggle<CR>

" Fzf
"nnoremap <leader><leader> :GFiles<CR>
nnoremap <leader>fi       :Files<CR>
nnoremap <leader>C        :Colors<CR>
"nnoremap <leader><CR>     :Buffers<CR>
"nnoremap <leader>fl       :Lines<CR>
nnoremap <leader>ag       :Ag! <C-R><C-W><CR>
nnoremap <leader>m        :History<CR>
nnoremap \ :Rg<CR>
nnoremap <C-T> :Files<cr>
nnoremap <Leader>b :Buffers<cr>
nnoremap <Leader>s :BLines<cr>

" Call flake8 on save buffer
autocmd BufWritePost *.py call flake8#Flake8()
autocmd FileType python cnoreabbrev <expr> q winnr("$") > 1 && getcmdtype() == ":" && getcmdline() == 'q' ? 'ccl <BAR> q' : 'q'

" restore place in file from previous session
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

nnoremap <F9> :Black<CR>

" Configure NerdTree
" file browser
let NERDTreeIgnore = ['\.pyc$', '__pycache__']
let NERDTreeMinimalUI = 1
let g:nerdtree_open = 0
"map <leader>n :NERDTreeToggle<CR>
set autochdir
let NERDTreeChDirMode=2
nnoremap <leader>n :NERDTree .<CR>
"function NERDTreeToggle()
    "NERDTreeTabsToggle
    "if g:nerdtree_open == 1
        "let g:nerdtree_open = 0
    "else
        "let g:nerdtree_open = 1
        "wincmd p
    "endif
"endfunction

let g:NERDCommentEmptyLines = 1

" Split line with Ctrl J
nnoremap <NL> i<CR><ESC>


noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

" disable autoindent when pasting text
" source: https://coderwall.com/p/if9mda/automatically-set-paste-mode-in-vim-when-pasting-in-insert-mode
let &t_SI .= "\<Esc>[?2004h"
let &t_EI .= "\<Esc>[?2004l"

function! XTermPasteBegin()
    set pastetoggle=<Esc>[201~
    set paste
    return ""
endfunction

inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()

"let g:airline_theme='onedark'
let g:airline_theme = "palenight"
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#hunks#enabled = 0

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
