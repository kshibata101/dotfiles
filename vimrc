"
" .vimrc for Vim >= 7.2
"
" Author: Yasunori Yusa
"

" vim
imap <c-j> <esc>

" disable vi compatible mode
set nocompatible

" enable modeline
set modeline

" set backup and swap configuration
set nobackup
set swapfile

" set directory configuration
set browsedir=buffer
set shellslash
if has('autochdir')
    set autochdir
endif

" set encoding
set encoding=utf-8
set fileformat=unix
set fileencodings=ucs-bom,iso-2022-jp,utf-8,euc-jp,sjis,default
set fileformats=unix,dos,mac

" set BS key
set backspace=indent,eol,start

" disable beep
set vb t_vb=

" enable syntax highlighting
if has('syntax')
    syntax on
endif

" set visual configuration
set title
set number
set ruler
set textwidth=0
set wrap
set ambiwidth=double
set showmatch
set list
set listchars=tab:^\ ,trail:_

" set indent style
set autoindent
set smartindent
set expandtab
set tabstop=8
set shiftwidth=4
set softtabstop=4

" set search configuration
set incsearch
set wrapscan
set ignorecase
set smartcase
set hlsearch

" set key bindings in insert mode
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-h> <BS>
inoremap <C-n> <Down>
inoremap <C-p> <Up>
nnoremap <C-g> <Esc>
vnoremap <C-g> <Esc>

" set status bar configuration
set laststatus=2
set statusline=%<%f\ %m%r%h%w
set statusline+=%{'['.(&fenc!=''?&fenc:&enc).'('.&fileformat.')]'}
set statusline+=%=%l/%L,%c%V%8P

" set command configuration
set wildmenu

" set color scheme
colorscheme koehler

" カーソル行をハイライト
set cursorline
:hi clear CursorLine
:hi CursorLine gui=underline
highlight CursorLine ctermbg=darkblue guibg=black
