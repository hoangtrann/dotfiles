set guicursor=
set number
set ai
set hlsearch
set ruler
set noerrorbells
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set autoindent
set smartindent
set cindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set incsearch
set clipboard=unnamed
set cmdheight=2
set colorcolumn=80
set scrolloff=3 " Keep 3 lines below and above the cursor
highlight ColorColumn ctermbg=0

filetype on
filetype indent plugin on
syntax on

" au! FileType python setl smartindent indentexpr=
au! FileType python setl smartindent

autocmd BufWritePre * :%s/\s\+$//e

call plug#begin('~/.config/nvim/plugged')

Plug 'morhetz/gruvbox'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'
Plug 'joshdick/onedark.vim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'vim-scripts/git-time-lapse'
Plug 'majutsushi/tagbar'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'neoclide/coc-neco'
Plug 'elzr/vim-json'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'mbbill/undotree'
Plug 'pangloss/vim-javascript'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'nvie/vim-flake8'
Plug 'psf/black', { 'branch': 'stable' }
" Plug 'fisadev/vim-isort'
Plug 'preservim/nerdtree'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'sheerun/vim-polyglot'
Plug 'qpkorr/vim-bufkill'
Plug 'prettier/vim-prettier'
Plug 'Yggdroot/indentLine'
Plug 'Vimjas/vim-python-pep8-indent'
call plug#end()

if !has('nvim')
  set ttymouse=xterm2
endif

" always show the status bar
set laststatus=2

" sane text files
set fileformat=unix
set encoding=utf-8
set fileencoding=utf-8
if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif

" let g:gruvbox_contrast_dark = 'hard'
" let g:palenight_terminal_italics=1
"
set background=dark
colorscheme onedark

if executable('rg')
    let g:rg_derive_root = 'true'
endif
set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ --follow
" let g:rg_derive_root='true'


lua << EOF
require'nvim-treesitter.configs'.setup {
        highlight = {
                enable = true,
        },
        refactor = {
                highlight_definitions = {
                        enable = true
                },
        },
        ensure_installed = 'python'
}
EOF

let mapleader = ","
let g:netrw_browse_split = 2
let g:netrw_banner = 0
let g:netrw_winsize = 25
let g:palenight_terminal_italics=1
let g:ctrlp_use_caching = 0

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
" nnoremap <Leader>rg :Rg<SPACE>
nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>

" Split window
" nmap ss :split<Return><C-w>w
" nmap sv :vsplit<Return><C-w>w

" move through split windows
nmap <leader><Up> :wincmd k<CR>
nmap <leader><Down> :wincmd j<CR>
nmap <leader><Left> :wincmd h<CR>
nmap <leader><Right> :wincmd l<CR>

" move through buffers
nmap <leader>[ :bp!<CR>
nmap <leader>] :bn!<CR>
nmap <leader>x :BD<CR>

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

" code folding
set foldmethod=indent
set foldlevel=99

" tag list
map <leader>t :TagbarToggle<CR>

" Fzf
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>fi :Files <C-R>=expand('%:h')<CR><CR>
nnoremap <leader>G :GFiles?<CR>
nnoremap <Leader>b :Buffers<cr>
nnoremap <Leader>s :BLines<cr>
nnoremap <leader>C :Colors<CR>
nnoremap <leader>ag :Ag! <C-R><C-W><CR>
nnoremap <leader>m :History<CR>
nnoremap \ :Rg<CR>
" nnoremap <leader>fi :Files<CR>
" nnoremap <C-T> :Files<cr>
"
" Call flake8 on save buffer
autocmd BufWritePost *.py call flake8#Flake8()
autocmd FileType python cnoreabbrev <expr> q winnr("$") > 1 && getcmdtype() == ":" && getcmdline() == 'q' ? 'ccl <BAR> q' : 'q'
let g:syntastic_python_flake8_config_file='.flake8'
nnoremap <C-K> :call flake8#Flake8ShowError()<cr>

" restore place in file from previous session
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

nnoremap <F9> :Black<CR>
let g:black_linelength = 80
" Configure NerdTree
" file browser
let NERDTreeIgnore = ['\.pyc$', '__pycache__']
let NERDTreeMinimalUI = 1
let g:nerdtree_open = 0
" map <leader>n :NERDTreeToggle<CR>
silent! nmap <C-p> :NERDTreeToggle<CR>
silent! map <F3> :NERDTreeFind<CR>

let g:NERDTreeMapActivateNode="<F3>"
let g:NERDTreeMapPreview="<F4>"

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

let g:airline#extensions#branch#enabled=1
let g:airline#parts#ffenc#skip_expected_string='utf-8[unix]'
let g:airline#extensions#branch#displayed_head_limit = 12

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
" let g:airline_left_sep = '»'
" let g:airline_left_sep = '▶'
" let g:airline_right_sep = '«'
" let g:airline_right_sep = '◀'
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

