call plug#begin('~/.config/nvim/plugged')

" Colorschemes and icons
Plug 'morhetz/gruvbox'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'
Plug 'joshdick/onedark.vim'
Plug 'haishanh/night-owl.vim'
Plug 'sainnhe/everforest'
Plug 'sainnhe/gruvbox-material'
Plug 'sainnhe/edge'
" Plug 'kyazdani42/nvim-web-devicons'
" Plug 'romgrk/barbar.nvim'
" Plug 'ryanoasis/vim-devicons'

Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'rbong/vim-flog'
Plug 'majutsushi/tagbar'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-surround'
Plug 'mbbill/undotree'
Plug 'prettier/vim-prettier', { 'do': 'npm install', 'branch': 'release/0.x' }

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Plug 'nvie/vim-flake8'
" Plug 'psf/black', { 'branch': 'stable' }
" Plug 'fisadev/vim-isort'
Plug 'Vimjas/vim-python-pep8-indent'

Plug 'preservim/nerdtree'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" Plug 'sheerun/vim-polyglot'
Plug 'qpkorr/vim-bufkill'

Plug 'Yggdroot/indentLine'

Plug 'othree/xml.vim'

call plug#end()

set shortmess=atIc
set nobackup
set backupcopy=yes " Fix file watchers

set guicursor=
set number
set relativenumber
set nu rnu
set nowrap
set title         " Set terminal window

set ai
" set nohlsearch
set incsearch
set ruler
set noerrorbells

set cursorline
set expandtab
set smarttab
set shiftwidth=2
set tabstop=2
set softtabstop=2
set autoindent

set statusline=%<%F%h%m%r%=\[%B\]\ %l,%c%V\ %P " Default status line. Largely here as a fallback if airline is not available
set laststatus=2
set backspace=indent,eol,start " Backspace over everything in insert mode
set ignorecase " Make searches case-insensitive..."
set smartcase
set noswapfile
set clipboard=unnamed

set showcmd
set cmdheight=2
set colorcolumn=80
set scrolloff=3 " Keep 3 lines below and above the cursor
set hidden

set signcolumn=number

set updatetime=300
set shortmess+=c

" set splitbelow

" highlight ColorColumn ctermbg=0
" highlight ColorColumn=

filetype indent plugin on
syntax on

if !has('nvim')
  set ttymouse=xterm2
endif

" sane text files
set encoding=utf-8
set fileencoding=utf-8

" Toggle between number and relativenumber
" function! ToggleNumber()
"   if(&relativenumber == 1)
"       set norelativenumber
"           set number
"       else
"           set relativenumber
"   endif
" endfunc
"
" map <Leader>tn :call ToggleNumber()<CR>


" I can type :help on my own, thanks.
noremap <F1> <Esc>

augroup configgroup
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

  " Some file types use real tabs
  autocmd FileType {make,gitconfig} setlocal noexpandtab sw=4

  " Treat JSON files like JavaScript
  autocmd BufNewFile,BufRead *.json setf javascript

  autocmd BufNewFile,BufRead *.xml setf html

  autocmd FileType javascript setlocal sts=4 ts=4 sw=4

  " Make Python follow PEP8
  autocmd FileType python setlocal sts=4 ts=4 sw=4

  autocmd FileType {xml,html} setlocal sts=4 ts=4 sw=4

  " Make sure all markdown files have the correct filetype
  autocmd BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown

  autocmd BufEnter Makefile setlocal noexpandtab

  autocmd BufWritePre * :%s/\s\+$//e

augroup END

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

let g:everforest_background = 'hard'
" let g:edge_style = 'neon'
" let g:airline_theme = 'onedark'
" let g:gruvbox_contrast_dark = 'hard'
" let g:palenight_terminal_italics=1

set background=dark
colorscheme everforest

if executable('rg')
    let g:rg_derive_root = 'true'
endif
set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ --follow

lua << EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",
  highlight = {
          enable = true,
  },
}
EOF
highlight link TSError Normal

set splitbelow splitright

let mapleader = ","

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nmap <leader><Up> :wincmd k<CR>
nmap <leader><Down> :wincmd j<CR>
nmap <leader><Left> :wincmd h<CR>
nmap <leader><Right> :wincmd l<CR>

" nmap <C-k> :wincmd k<CR>
" nmap <C-j> :wincmd j<CR>
" nmap <C-h> :wincmd h<CR>
" nmap <C-l> :wincmd l<CR>

" nmap ss :split<Return><C-w>w
" nmap sv :vsplit<Return><C-w>w
"
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>

nnoremap <Leader>rg :Rg <C-R><C-W><CR>
nnoremap <leader>u :UndotreeShow<CR>

" move through buffers
nmap <leader>[ :bp!<CR>
nmap <leader>] :bn!<CR>
nmap <leader>x :BD<CR>

" word movement
" imap <S-Left> <Esc>bi
" nmap <S-Left> b
" imap <S-Right> <Esc><Right>wi
" nmap <S-Right> w

" indent/unindent with tab/shift-tab
" nmap <Tab> >>
" imap <S-Tab> <Esc><<i
" nmap <S-tab> <<

" mouse
set mouse=a
let g:is_mouse_enabled = 1

" code folding
set foldmethod=indent
set foldlevel=99

" tag list
map <leader>t :TagbarToggle<CR>

" Fzf
" nnoremap <leader><leader> :Files<CR>
nnoremap <C-T> :Files<cr>
nnoremap <leader>fi :Files <C-R>=expand('%:h')<CR><CR>
nnoremap <leader>G :GFiles?<CR>
nnoremap <Leader>B :Buffers<cr>
nnoremap <Leader>s :BLines<cr>
nnoremap <leader>C :Colors<CR>
nnoremap <leader>ag :Ag! <C-R><C-W><CR>
nnoremap <leader>m :History<CR>
nnoremap \ :Rg<CR>

" let g:fzf_layout = { 'down':  '40%'}

" Call flake8 on save buffer
" autocmd BufWritePost *.py call flake8#Flake8()
" autocmd FileType python cnoreabbrev <expr> q winnr("$") > 1 && getcmdtype() == ":" && getcmdline() == 'q' ? 'ccl <BAR> q' : 'q'
"
" let g:syntastic_python_flake8_config_file='.flake8'
" nnoremap <C-K> :call flake8#Flake8ShowError()<cr>

let g:python_host_prog = '/home/ryan/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '/home/ryan/.pyenv/versions/neovim3/bin/python'

" let g:vim_isort_python_version = 'python3'
" let s:available_short_python = ':py3'
" let g:vim_isort_map = '<C-i>'

" let python_highlight_space_errors = 0
" let g:pymode_syntax_space_errors = 0

" Configure NerdTree
" file browser
let g:netrw_browse_split = 2
let g:netrw_banner = 0
let g:netrw_winsize = 40

let NERDTreeIgnore = ['\.pyc$', '__pycache__']
" let NERDTreeMinimalUI = 1
let g:nerdtree_open = 0

silent! nmap <C-p> :NERDTreeToggle<CR>
silent! nmap <F3> :NERDTreeFind<cr>
" let g:NERDTreeMapActivateNode="<F3>"
" let g:NERDTreeMapPreview="<F4>"

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

let g:airline#extensions#branch#enabled=1
let g:airline#parts#ffenc#skip_expected_string='utf-8[unix]'
let g:airline#extensions#branch#displayed_head_limit = 12
let g:airline#extensions#tabline#enabled = 0
let g:airline_powerline_fonts = 1

" Append the character code to airline_section_z
" let g:airline_section_z = airline#section#create(['windowswap', '%3p%%', 'linenr', ':%3v', ' | 0x%2B'])
let g:airline#extensions#coc#enabled = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
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
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" <TAB>: completion.
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

let g:coc_global_extensions = [
  \'coc-pyright',
  \'coc-eslint',
  \'coc-snippets',
  \'coc-emoji',
  \'coc-json',
  \'coc-css',
  \'coc-html',
  \'coc-yaml',
  \'coc-prettier'
  \]

if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" " Use `:CocDiagnostics` to get all diagnostics of current buffer in location
" list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

autocmd CursorHold * silent call CocActionAsync('highlight')

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Symbol renaming.
nmap <Leader>rn <Plug>(coc-rename)

" Formatting selected code.
" xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')
" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)
" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

nmap <silent> <F9> :call CocAction('format')<CR>

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
    let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let bufferline = get(g:, 'bufferline', {})
let bufferline.animation = v:false
let bufferline.auto_hide = v:true
let bufferline.icons = v:false
