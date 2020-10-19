" -----------------------------------------------------------------------------
"                                   .vimrc
" -----------------------------------------------------------------------------

"                                  PLUGINS
"                                  =======

set background=dark
let base16colorspace=256
set termguicolors
filetype off
call plug#begin('~/.vim/plugged')

Plug 'itchyny/lightline.vim'
Plug 'chriskempson/base16-vim'
Plug 'pangloss/vim-javascript'
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'mxw/vim-jsx'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'luochen1990/rainbow'
Plug 'w0rp/ale'
Plug 'flowtype/vim-flow'
Plug 'junegunn/goyo.vim'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'tpope/vim-fugitive'
Plug 'cloudhead/neovim-ghcid'

Plug 'ptzz/lf.vim'
Plug 'rbgrouleff/bclose.vim'

" Plug 'mattn/emmet-vim', { 'for': 'javascript' }
" Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" Plug 'racer-rust/vim-racer'

Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'liuchengxu/vim-clap'

Plug 'lilydjwg/colorizer'
Plug 'prettier/vim-prettier'
Plug 'meck/vim-brittany'

Plug 'tpope/vim-rsi'

Plug 'neovimhaskell/haskell-vim'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': './install.sh'
    \ }

call plug#end()
filetype plugin indent on

"                                  MAPPING
"                                  =======

let mapleader=" "
let maplocalleader=","

" quick command in insert mode
inoremap II <Esc>I
inoremap AA <Esc>A
inoremap OO <Esc>O
inoremap fd <Esc>

" c-p to open files
nnoremap <C-p> :Files<CR>

nnoremap H 0
nnoremap L $
vnoremap H 0
vnoremap L $

" window split
nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s

nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k

nnoremap ( %
vnoremap ( %


" copy selection to clipboard
vnoremap <c-y> "+y

" use fzf to search inside files
let g:fzf_layout = { 'down': '~80%' }
nnoremap <c-f> :Find<space>
nnoremap <c-b> :Buffers<CR>
nnoremap <Leader>f :Find<space><C-r><C-w><cr>
command! -bang -nargs=* Find call fzf#vim#grep(
      \ 'rg --column --line-number --no-heading --fixed-strings --smart-case --hidden --follow --color "always" '.shellescape(<q-args>),
      \ 1,
      \ fzf#vim#with_preview(),
      \ <bang>0
      \ )

" custom files preview
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, <bang>0)

" grep in current project & navigate in results
" use silver searcher if possible
command -nargs=+ -complete=file -bar Grep silent! grep! "<args>" | cwindow | redraw!
nnoremap \ :Grep<SPACE>
vnoremap \ y:Grep<SPACE><C-R><C-0><CR>
nnoremap <Leader>n :cnext<CR>
nnoremap <Leader>t :cprevious<CR>
nnoremap <Leader>q :ccl<CR>
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif

" indent { / [ / ( & put cursor on blank line when <Enter> inside
inoremap {<cr> {<cr>}<c-o>O
inoremap [<cr> [<cr>]<c-o>O
inoremap (<cr> (<cr>)<c-o>O

command Sp set paste
command Np set nopaste

"                                  SETTINGS
"                                  ========

" different cursors based on modes
" set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" need explanation ?
syntax on

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

set mouse=c

" encoding, etc.
set encoding=utf-8
set termencoding=utf-8

" correct strange bug
set backspace=indent,eol,start

" insert space characters whenever <tab> is pressed
set expandtab

" number of spaces inserted when hitting <tab>
set tabstop=2

" number of spaces inserted when using :retab
set shiftwidth=2

" don't wrap long lines
set nowrap

" always show status bar
set laststatus=2

" number of lines to keep above & below cursor when scrolling
set sidescrolloff=15
set sidescroll=1

" auto reload files when changed
set autoread

" show the 80 chars column
" set colorcolumn=80

" don't create useless files
set noswapfile
set nobackup
set nowb

" hide unsaved buffers
set hidden

" show cursor line
set cursorline

" move on search
set incsearch

" show line numbers
set relativenumber
set number

" show infos in status bar
set ruler

" prevent annoying highlight on search
set nohlsearch

" more intelligent searches
set ignorecase
set smartcase

" never use Ex useless mode
nnoremap Q <ESC>

" show blank characters
set listchars=tab:>-,trail:·,nbsp:%
set list

" transparent bg
hi Normal ctermbg=NONE

" wild menu completion
set wildmode=longest,full
set wildignore=*.o,*.obj,*~
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*node_modules*
set wildignore+=*ios/*
set wildignore+=yarn.lock
set wildignore+=*android/*
set wildignore+=*bower_components*
set wildignore+=*plugins*
set wildignore+=*platforms*
set wildignore+=*release*
set wildignore+=*dist*,*dist-server*,*lib*
set wildmenu

" Re-indent the whole buffer.
function! Indent()
  call Preserve('normal gg=G')
endfunction

" indentation by language
autocmd BufNewFile,BufRead *.gyp set syntax=javascript
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.tsx
autocmd BufNewFile,BufRead *.c set formatprg=indent\ -kr\ -ts4
autocmd BufWritePre *.c :normal gg=G


" highlight for json5
au BufNewFile,BufRead *.json5 set filetype=javascript

" quickfix split
autocmd! FileType qf nnoremap <buffer> <leader><Enter> <C-w><Enter><C-w>L

" display extra spaces in red
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" resize panels when client is resized
autocmd VimResized * :wincmd =

" disable xml different color for closing tag
highlight link xmlEndTag xmlTag

" remember last used position
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

"                               PLUGINS CONFIG
"                               ==============

" lightline
let g:lightline = {
      \   'colorscheme': 'deus',
      \   'active': {
      \     'left': [ [ 'mode', 'paste' ], [ 'readonly', 'relativepath', 'modified' ] ],
      \     'right': [ [ 'lineinfo' ], [ 'linter_warnings', 'linter_errors', 'linter_ok' ] ]
      \    },
      \   'inactive': {
      \     'left': [ ['relativepath' ] ],
      \     'right': []
      \    },
      \   'component_expand': {
      \     'linter_warnings': 'LightlineLinterWarnings',
      \     'linter_errors': 'LightlineLinterErrors',
      \     'linter_ok': 'LightlineLinterOK'
      \   },
      \   'component_type': {
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'ok'
      \   },
      \ }

autocmd User ALELint call lightline#update()

" ale + lightline
function! LightlineLinterWarnings() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf('%d', all_non_errors)
endfunction

function! LightlineLinterErrors() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf('✖ %d', all_errors)
endfunction

function! LightlineLinterOK() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '✓' : ''
endfunction


" vim jsx
let g:jsx_ext_required = 0

" nerdcommenter
let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1
let g:NERDDefaultAlign = 'left'

"prettier
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync
" let g:prettier#config#print_width = 80
" let g:prettier#config#single_quote = 'false'
" let g:prettier#config#bracket_spacing = 'true'
" let g:prettier#config#trailing_comma = 'none'
" let g:prettier#config#jsx_bracket_same_line = 'false'


"false ale
let g:ale_linters = { 'javascript': ['eslint', 'flow'] }
let g:ale_fixers = { 'javascript': ['eslint'] }
let g:ale_sign_error = '✖'
let g:ale_sign_warning = 'ℹ'
let g:ale_set_highlights = 0

nnoremap <Leader>d :ALEDetail<CR>

" rainbow parenthesis
let g:rainbow_active = 1


" terminal escape mode
tnoremap <Esc> <C-\><C-n>


" nnoremap <C-=> :vsplit term://zsh
" let g:slime_target = "neovim"


" autocmd FileType haskell nnoremap <buffer> <leader>? :call ale#cursor#ShowCursorDetail()<cr>
"
nnor ,fn :let @"=expand("%")<CR>      " Mnemonic: yank File Name
hi Normal guibg=NONE ctermbg=NONE


let g:lf_map_keys = 0
map <leader>F :Lf<CR>


let g:flow#flowpath = "/home/flocks/ledger/ledger-vault-front/node_modules/.bin/flow"

