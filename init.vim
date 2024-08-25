call plug#begin('~/.config/nvim/plugged')

" Colorschemes and icons
Plug 'arcticicestudio/nord-vim', { 'as': 'nord' }
Plug 'catppuccin/nvim', { 'as': 'catppuccin', 'branch': 'main' }
Plug 'rose-pine/neovim', { 'as': 'rose-pine', 'branch': 'main'}
Plug 'EdenEast/nightfox.nvim'

Plug 'kyazdani42/nvim-web-devicons'
Plug 'ryanoasis/vim-devicons'

Plug 'editorconfig/editorconfig-vim'
Plug 'hoob3rt/lualine.nvim', {'branch': 'master'}

Plug 'airblade/vim-rooter'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'

Plug 'mbbill/undotree'
Plug 'prettier/vim-prettier', { 'do': 'npm install', 'branch': 'release/0.x' }
Plug 'mechatroner/rainbow_csv'

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
Plug 'WhoIsSethDaniel/toggle-lsp-diagnostics.nvim'
Plug 'SirVer/ultisnips'

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

Plug 'windwp/nvim-autopairs'

Plug 'SmiteshP/nvim-navic'
Plug 'MunifTanjim/nui.nvim'
Plug 'SmiteshP/nvim-navbuddy'

Plug 'fgheng/winbar.nvim'


call plug#end()

set shortmess=atIc
set nobackup
set backupcopy=yes " Fix file watchers

set guicursor=
set number
set relativenumber
set nu
set nowrap
set title         " Set terminal window

set ai
set nohlsearch
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
set laststatus=3
set backspace=indent,eol,start " Backspace over everything in insert mode
set ignorecase " Make searches case-insensitive..."
set smartcase
set noswapfile
set clipboard=unnamed

set showcmd
set cmdheight=2
" set colorcolumn=88
set scrolloff=8 " Keep 8 lines below and above the cursor
set hidden

set signcolumn=number

set updatetime=300
set shortmess+=c
set completeopt=menu,menuone,noselect

" set list
" set listchars=eol:↵
" set listchars=eol:↵
" set listchars=eol:↴

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
    theme = 'rose-pine',
    -- section_separators = {left = '', right = ''},
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
      { "filename", file_status = true, path = 1 }
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
  winbar = {
        lualine_c = {
            {
                "navic",
                color_correction = nil,
                navic_opts = nil
            }
        }
    }
})

require('telescope').setup({
  defaults = {
    -- layout_strategy = 'vertical',
    layout_strategy = 'bottom_pane',
    sorting_strategy = 'ascending',
    scroll_strategy = 'limit',
    layout_config = {
      vertical = {
        height = 0.9,
        preview_cutoff = 40,
        prompt_position = "top",
        width = 0.9
      }
    },
    winblend = 0,
    file_ignore_patterns = { 'tags', 'i18n', 'log' },
  },
})



require('nvim-autopairs').setup()

require('ibl').setup({
  scope = {
      enabled = false,
    },
})

local navic = require("nvim-navic")

require'nvim-tree'.setup {
  hijack_cursor = true,
  view = {
    side = "left",
    signcolumn = "yes",
    width = 40,
  },
  renderer = {
    indent_width = 2,
    indent_markers = {
      enable = true,
      inline_arrows = true,
      icons = {
        corner = "└",
        edge = "│",
        item = "│",
        bottom = "─",
        none = " ",
      },
    },
  },
  actions = {
    open_file = {
      quit_on_open = true,
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
}


-- require('formatting')
require'lspconfig'.pyright.setup{
  on_attach = on_attach,
  settings = {
    pyright = {
      autoImportCompletion = true,
    },
    python = {
      analysis = {
        autoSearchPaths = true,
        diagnosticMode = 'openFilesOnly',
        useLibraryCodeForTypes = true,
        typeCheckingMode = 'off',
      }
    }
  }
}
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')


  if client.server_capabilities.documentSymbolProvider then
      navic.attach(client, bufnr)
  end

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
    { name = 'ultisnips', keyword_length = 6, group_index = 1, max_item_count = 30 },
    { name = 'nvim_lsp', keyword_length = 6, group_index = 1, max_item_count = 30 },
    { name = 'path', keyword_length = 6, group_index = 1, max_item_count = 30 },
    { name = 'buffer', keyword_length = 6, group_index = 1, max_item_count = 30 },
  })
})
require'toggle_lsp_diagnostics'.init()

-- require("catppuccin").setup({
--  flavour = "macchiato"
--})

-- require('rose-pine').setup({
--   dark_variant = 'main',
--   dim_inactive_windows = true,
--   highlight_groups = {
--       -- Comment = { fg = "foam" },
--       -- VertSplit = { fg = "muted", bg = "muted" },
--   },
-- })

vim.cmd.colorscheme "rose-pine"
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

nnoremap <leader>ff :lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg :lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fs :lua require('telescope.builtin').grep_string()<cr>
nnoremap <leader>fb :lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh :lua require('telescope.builtin').help_tags()<cr>
nnoremap <leader>ft :lua require('telescope.builtin').lsp_document_symbols({ignore_symbols = variable})<cr>

nnoremap \ :Rg<CR>

" mouse
set mouse=a
let g:is_mouse_enabled = 1

" code folding
set foldmethod=indent
set foldlevel=99


" let g:python_host_prog = '/home/ryan/.pyenv/versions/neovim2/bin/python'
" let g:python3_host_prog = '/Users/ryan/.pyenv/versions/neovim3/bin/python'
let g:python3_host_prog = '/opt/homebrew/bin/python3.10'

nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>r :NvimTreeRefresh<CR>
nnoremap <leader>n :NvimTreeFindFile<CR>

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

nnoremap <C-u> <C-u>zz
nnoremap <C-d> <C-d>zz
nnoremap n nzz
nnoremap N Nzz
