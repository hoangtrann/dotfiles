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
Plug 'lukas-reineke/indent-blankline.nvim'

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
set nohlsearch
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
set cmdheight=1
set colorcolumn=80
set scrolloff=8 " Keep 8 lines below and above the cursor
set hidden

set signcolumn=number

set updatetime=300
set shortmess+=c
set completeopt=menu,menuone,noselect

" set list
" set listchars=eol:↵
" set listchars=eol:↵
"set listchars=eol:↴

" set splitbelow

filetype indent plugin on
syntax on

if !has('nvim')
  set ttymouse=xterm2
endif

" sane text files
set encoding=utf-8
set fileencoding=utf-8

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

" let g:indentLine_char_list = ['▏', '|', '¦', '┆', '┊']
let g:indentLine_char_list = ['▏']

set background=dark

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
    -- section_separators = {left = '', right = '# '},
    section_separators = {left = '', right = ''},
    -- component_separators = {left = '', right = ''}
    -- section_separators = {left = '', right = ''},
    component_separators = {left = '', right = ''}
  },
  sections = {
    lualine_a = {"mode"},
    lualine_b = {"diff", "diagnostics"},
    lualine_c = {
      { "filetype", icon_only = true },
      {"filename", file_status = true,}
    },
    lualine_x = {
      "branch",
      "encoding",
      "fileformat"
    },
    lualine_y = {"progress"},
    lualine_z = {"location"}
  },
  tabline = {},
  winbar = {}
})

require('telescope').setup({
  defaults = {
    layout_strategy = 'vertical',
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
  auto_reload_on_write = true,
  disable_netrw = false,
  hijack_cursor = true,
  hijack_netrw = true,
  hijack_unnamed_buffer_when_opening = false,
  ignore_buffer_on_setup = false,
  open_on_setup = false,
  open_on_tab = false,
  sort_by = "name",
  update_cwd = false,
  view = {
    width = 35,
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

require("indent_blankline").setup {
    -- for example, context is off by default, use this to turn it on
    -- show_current_context = true,
    -- show_current_context_start = true,
    -- show_end_of_line = true,
}

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

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- Setup nvim-cmp.
  local cmp = require'cmp'
  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
    },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    mapping = {
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { "i", "s" }),
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

nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fs <cmd>Telescope grep_string<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
nnoremap \ :Rg<CR>

" let g:python_host_prog = '/home/ryan/.pyenv/versions/neovim2/bin/python'
" let g:python3_host_prog = '/Users/ryan/.pyenv/versions/neovim3/bin/python'
let g:python3_host_prog = '/opt/homebrew/bin/python3'

nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>r :NvimTreeRefresh<CR>
nnoremap <leader>n :NvimTreeFindFile<CR>

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

nnoremap n nzz
nnoremap N Nzz
