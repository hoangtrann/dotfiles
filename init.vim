call plug#begin('~/.config/nvim/plugged')

" Colorschemes and icons
Plug 'drewtempelmeyer/palenight.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'joshdick/onedark.vim'
Plug 'sainnhe/everforest'
Plug 'sainnhe/gruvbox-material'
Plug 'hoob3rt/lualine.nvim', {'branch': 'master'}
Plug 'rose-pine/neovim', { 'branch': 'main' }
Plug 'kyazdani42/nvim-web-devicons'
Plug 'ryanoasis/vim-devicons'
Plug 'editorconfig/editorconfig-vim'
Plug 'catppuccin/nvim', { 'as': 'catppuccin', 'branch': 'main' }

Plug 'airblade/vim-rooter'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'

Plug 'mbbill/undotree'
Plug 'prettier/vim-prettier', { 'do': 'npm install', 'branch': 'release/0.x' }
Plug 'mechatroner/rainbow_csv'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'akretion/vim-odoo-snippets'

Plug 'Vimjas/vim-python-pep8-indent'

Plug 'nvim-treesitter/nvim-treesitter', { 'branch': 'master', 'do': ':TSUpdate'}

Plug 'qpkorr/vim-bufkill'
Plug 'Yggdroot/indentLine'

Plug 'othree/xml.vim'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'neovim/nvim-lspconfig'
Plug 'glepnir/lspsaga.nvim', { 'branch': 'main' }
Plug 'mhartington/formatter.nvim'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-path'
Plug 'quangnguyen30192/cmp-nvim-ultisnips'
Plug 'WhoIsSethDaniel/toggle-lsp-diagnostics.nvim'

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

Plug 'windwp/nvim-autopairs'

call plug#end()

set shortmess=atIc
set nobackup
set backupcopy=yes " Fix file watchers

set guicursor=
set number
" set relativenumber
" set nu
set nowrap
set title         " Set terminal window

set ai
" set nohlsearch
set incsearch
set ruler
set noerrorbells

" set cursorline
set expandtab
set smarttab
set shiftwidth=2
set tabstop=2
set softtabstop=2
set autoindent

set statusline=%<%F%h%m%r%=\[%B\]\ %l,%c%V\ %P " Default status line. Largely here as a fallback if airline is not available
set laststatus=3
set backspace=indent,eol,start " Backspace over everything in insert mode
set ignorecase " Make searches case-insensitive..."
set smartcase
set noswapfile
set clipboard=unnamed

set showcmd
set cmdheight=2
set colorcolumn=80
set scrolloff=8 " Keep 8 lines below and above the cursor
set hidden

set signcolumn=number

set updatetime=300
set shortmess+=c
set completeopt=menu,menuone,noselect
" set listchars=eol:↵
" set listchars=eol:↵
" set list

" set splitbelow

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

" let g:everforest_background = 'hard'
" let g:everforest_enable_italic = 0
" let g:everforest_disable_italic_comment = 1

" let g:gruvbox_material_enable_italic = 0
" let g:gruvbox_material_background = 'hard'
" let g:gruvbox_material_diagnostic_text_highlight = 1
" let g:gruvbox_material_diagnostic_line_highlight = 1

" let g:dracula_italic = 0

" let g:tokyonight_style = "night"
" let g:tokyonight_italic_functions = 0
" let g:tokyonight_italic_comments = 0
" let g:tokyonight_italic_keywords = 0
" let g:tokyonight_italic_variables = 0
"
" let g:rose_pine_disable_italics = 1
" let g:rose_pine_bold_vertical_split_line = 1
" let g:catppuccin_flavour = "mocha"

set background=dark
" colorscheme rose-pine

if executable('rg')
    let g:rg_derive_root = 'true'
endif
set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ --follow

lua << EOF

require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",
  ignore_install = {"phpdoc"},
  highlight = {
          enable = true,
  },
}
require('lualine').setup({
  options = {
    theme = 'catppuccin',
    -- section_separators = {left = '', right = ''},
    -- component_separators = {left = '', right = ''}
    section_separators = {left = '', right = ''},
    component_separators = {left = '', right = ''}
  },
  sections = {
    lualine_a = {"mode"},
    lualine_b = {"branch"},
    lualine_c = {
      {"filetype", icon_only = true},
      "filename"
      },
    lualine_x = {
      "encoding",
      "fileformat"
    },
    lualine_y = {"progress"},
    lualine_z = {"location"}
  }
})
require('telescope').setup({
  defaults = {
    layout_strategy = 'horizontal',
    layout_config = {
      horizontal = { width = 0.95 },
    },
    scroll_strategy = 'cycle',
    winblend = 0,
    file_ignore_patterns = { 'tags', 'i18n', 'log' },
  },
})
-- require('neogit').setup({})
-- require('nvim-autopairs').setup()

require'nvim-tree'.setup { -- BEGIN_DEFAULT_OPTS
  auto_close = false,
  auto_reload_on_write = true,
  disable_netrw = false,
  hide_root_folder = false,
  hijack_cursor = true,
  hijack_netrw = true,
  hijack_unnamed_buffer_when_opening = false,
  ignore_buffer_on_setup = false,
  open_on_setup = false,
  open_on_tab = false,
  sort_by = "name",
  update_cwd = false,
  view = {
    width = 30,
    height = 30,
    side = "left",
    preserve_window_proportions = false,
    number = false,
    relativenumber = false,
    signcolumn = "yes",
    mappings = {
      custom_only = false,
      list = {
        -- user mappings go here
      },
    },
  },
  hijack_directories = {
    enable = true,
    auto_open = true,
  },
  update_focused_file = {
    enable = false,
    update_cwd = false,
    ignore_list = {},
  },
  ignore_ft_on_setup = {},
  system_open = {
    cmd = nil,
    args = {},
  },
  diagnostics = {
    enable = false,
    show_on_dirs = false,
    icons = {
      hint = "",
      info = "",
      warning = "",
      error = "",
    },
  },
  filters = {
    dotfiles = false,
    custom = {},
    exclude = {},
  },
  git = {
    enable = true,
    ignore = true,
    timeout = 400,
  },
  actions = {
    change_dir = {
      enable = true,
      global = false,
    },
    open_file = {
      quit_on_open = false,
      resize_window = true,
      window_picker = {
        enable = true,
        chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
        exclude = {
          filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
          buftype = { "nofile", "terminal", "help" },
        },
      },
    },
  },
  trash = {
    cmd = "trash",
    require_confirm = true,
  },
  log = {
    enable = false,
    truncate = false,
    types = {
      all = false,
      config = false,
      git = false,
    },
  },
} -- END_DEFAULT_OPTS

-- require('github-theme').setup({
--   comment_style = 'NONE'
-- })
-- require("bufferline").setup({})

-- require('rose-pine').setup({
--   dark_variant = 'moon'
-- })

-- require('rose-pine.functions').select_variant('moon')

-- require('formatting')
require'lspconfig'.pyright.setup{}
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>o', '<cmd>lua vim.lsp.buf.organizeImports()<CR>', opts)

end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { 'pyright', 'rust_analyzer', 'tsserver' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end

local filetypes = {
  typescript = "eslint",
  typescriptreact = "eslint",
  python = "flake8",
  php = {"phpcs", "psalm"},
}

local linters = {
  eslint = {
    sourceName = "eslint",
    command = "./node_modules/.bin/eslint",
    rootPatterns = {".eslintrc.js", "package.json"},
    debouce = 100,
    args = {"--stdin", "--stdin-filename", "%filepath", "--format", "json"},
    parseJson = {
      errorsRoot = "[0].messages",
      line = "line",
      column = "column",
      endLine = "endLine",
      endColumn = "endColumn",
      message = "${message} [${ruleId}]",
      security = "severity"
    },
    securities = {[2] = "error", [1] = "warning"}
  },
  flake8 = {
    command = "flake8",
    sourceName = "flake8",
    args = {"--format", "%(row)d:%(col)d:%(code)s: %(text)s", "%file"},
    formatPattern = {
      "^(\\d+):(\\d+):(\\w+):(\\w).+: (.*)$",
      {
          line = 1,
          column = 2,
          message = {"[", 3, "] ", 5},
          security = 4
      }
    },
    securities = {
      E = "error",
      W = "warning",
      F = "info",
      B = "hint",
    },
  }
}

nvim_lsp.diagnosticls.setup {
  on_attach = on_attach,
  filetypes = vim.tbl_keys(filetypes),
  init_options = {
    filetypes = filetypes,
    linters = linters,
  },
}

require('formatter').setup({

  loggin = true,

  filetype = {
    python = {
      function()
        return {exe = "black", args = { "-" }, stdin = true}
      end,
    }
  }
})

-- Setup nvim-cmp.
  local cmp = require'cmp'
  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    mapping = {
      ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
      ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
    },
    sources = cmp.config.sources({
      { name = 'nvim_lsp', keyword_length = 2 },
      { name = 'ultisnips' },
      { name = 'path', keyword_length = 2 },
      { name = 'buffer', keyword_length = 2 },
    })
  })
require'toggle_lsp_diagnostics'.init()

require("catppuccin").setup()

vim.g.catppuccin_flavour = "mocha"
vim.cmd('colorscheme catppuccin')
EOF

highlight link TSError Normal

set splitbelow splitright

let mapleader = ","

nmap <leader>tt  <Plug>(toggle-lsp-diag)
" nnoremap <silent> <leader>F :Format<CR>

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nmap <leader><Up> :wincmd k<CR>
nmap <leader><Down> :wincmd j<CR>
nmap <leader><Left> :wincmd h<CR>
nmap <leader><Right> :wincmd l<CR>

nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>

nnoremap <Leader>rg :Rg <C-R><C-W><CR>
nnoremap <leader>u :UndotreeShow<CR>

" move through buffers
nmap <leader>[ :bp!<CR>
nmap <leader>] :bn!<CR>
nmap <leader>x :BD<CR>

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
" map <leader>t :TagbarToggle<CR>

nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fs <cmd>Telescope grep_string<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Fzf
" nnoremap <leader><leader> :Files<CR>
" nnoremap <C-T> :Files<cr>
" nnoremap <leader>fi :Files <C-R>=expand('%:h')<CR><CR>
" nnoremap <leader>G :GFiles?<CR>
" nnoremap <Leader>B :Buffers<cr>
" nnoremap <Leader>s :BLines<cr>
" nnoremap <leader>C :Colors<CR>

" nnoremap <leader>ag :Ag! <C-R><C-W><CR>
" nnoremap <leader>m :History<CR>
nnoremap \ :Rg<CR>

" let g:fzf_layout = { 'down':  '40%'}

" Call flake8 on save buffer
" autocmd BufWritePost *.py call flake8#Flake8()
" autocmd FileType python cnoreabbrev <expr> q winnr("$") > 1 && getcmdtype() == ":" && getcmdline() == 'q' ? 'ccl <BAR> q' : 'q'
"
" let g:syntastic_python_flake8_config_file='.flake8'
" nnoremap <C-K> :call flake8#Flake8ShowError()<cr>

" let g:python_host_prog = '/home/ryan/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '/Users/ryan/.pyenv/versions/neovim3/bin/python'

" Configure NerdTree
" file browser
" let g:netrw_browse_split = 2
" let g:netrw_banner = 0
" let g:netrw_winsize = 40

" let NERDTreeIgnore = ['\.pyc$', '__pycache__']
" let NERDTreeMinimalUI = 1
" let g:nerdtree_open = 0

" silent! nmap <C-p> :NERDTreeToggle<CR>
" silent! nmap <F3> :NERDTreeFind<cr>
" let g:NERDTreeMapActivateNode="<F3>"
" let g:NERDTreeMapPreview="<F4>"

nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>r :NvimTreeRefresh<CR>
nnoremap <leader>n :NvimTreeFindFile<CR>

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

" let g:airline#extensions#branch#enabled=1
" let g:airline#parts#ffenc#skip_expected_string='utf-8[unix]'
" let g:airline#extensions#branch#displayed_head_limit = 12
" let g:airline#extensions#tabline#enabled = 0
" let g:airline_powerline_fonts = 1

" Append the character code to airline_section_z
" let g:airline_section_z = airline#section#create(['windowswap', '%3p%%', 'linenr', ':%3v', ' | 0x%2B'])
" let g:airline#extensions#coc#enabled = 1

" if !exists('g:airline_symbols')
"   let g:airline_symbols = {}
" endif

" unicode symbols
" let g:airline_symbols.crypt = '🔒'
" let g:airline_symbols.linenr = '␊'
" let g:airline_symbols.linenr = '␤'
" let g:airline_symbols.linenr = '¶'
" let g:airline_symbols.maxlinenr = '☰'
" let g:airline_symbols.maxlinenr = ''
" let g:airline_symbols.branch = '⎇'
" let g:airline_symbols.paste = 'ρ'
" let g:airline_symbols.paste = 'Þ'
" let g:airline_symbols.paste = '∥'
" let g:airline_symbols.spell = 'Ꞩ'
" let g:airline_symbols.notexists = '∄'
" let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
" let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
" let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''
" let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
" let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''
" let g:airline_symbols.branch = ''
" let g:airline_symbols.readonly = ''
" let g:airline_symbols.linenr = ''


" let g:coc_global_extensions = [
"   \'coc-pyright',
"   \'coc-eslint',
"   \'coc-snippets',
"   \'coc-emoji',
"   \'coc-json',
"   \'coc-css',
"   \'coc-html',
"   \'coc-yaml',
"   \'coc-prettier'
"   \]

" Use `[g` and `]g` to navigate diagnostics
" " Use `:CocDiagnostics` to get all diagnostics of current buffer in location
" list.
" nmap <silent> [g <Plug>(coc-diagnostic-prev)
" nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
" nnoremap <silent> K :call <SID>show_documentation()<CR>

" autocmd CursorHold * silent call CocActionAsync('highlight')

" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   elseif (coc#rpc#ready())
"     call CocActionAsync('doHover')
"   else
"     execute '!' . &keywordprg . " " . expand('<cword>')
"   endif
" endfunction

" Symbol renaming.
" nmap <Leader>rn <Plug>(coc-rename)

" Formatting selected code.
" xmap <leader>f <Plug>(coc-format-selected)
" nmap <leader>f <Plug>(coc-format-selected)

" Add `:Format` command to format current buffer.
" Add `:Fold` command to fold current buffer.
" command! -nargs=? Fold :call     CocAction('fold', <f-args>)
" Add `:OR` command for organize imports of the current buffer.
" command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" nmap <silent> <F9> :call CocAction('format')<CR>

" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
"
" function! s:check_back_space() abort
"     let col = col('.') - 1
"       return !col || getline('.')[col - 1]  =~# '\s'
" endfunction

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.

" <TAB>: completion.
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" Use <c-space> to trigger completion.
" if has('nvim')
"   inoremap <silent><expr> <c-space> coc#refresh()
" else
"   inoremap <silent><expr> <c-@> coc#refresh()
" endif
"
" inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"                               \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
"
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" if has('nvim-0.4.0') || has('patch-8.2.0750')
"   nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
"   nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
"   inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
"   inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
"   vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
"   vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
" endif

" Mappings for CoCList
" nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

nnoremap n nzz
nnoremap N Nzz

