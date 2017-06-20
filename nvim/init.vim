syntax on
set background=dark
colorscheme solarized
"###有効かわからないためコメントアウト###
"set t_Co=256
set number
set list
set listchars=tab:>>,trail:-,nbsp:%,eol:↲
set expandtab
set autoindent
set shiftwidth=4
filetype on            " enables filetype detection
filetype plugin on     " enables filetype specific plugins
"###Pythonのシンタックスハイライトが半端っぽいので追加###
let python_highlight_all = 1

"###括弧の自動補完の設定を入れてみる###
inoremap { {}<LEFT>
inoremap ( ()<LEFT>
inoremap [ []<LEFT>

"#####dein.vimの設定#####
if &compatible
  set nocompatible
  endif
  set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state(expand('~/.vim/dein'))
  call dein#begin(expand('~/.vim/dein'))

" プラグインリストを収めたTOMLファイル
  let g:dein_dir = expand('~/.vim/dein')
  let s:toml = g:dein_dir . '/dein.toml'
  let s:lazy_toml = g:dein_dir . '/dein_lazy.toml'

" TOMLファイルにpluginを記述
  call dein#load_toml(s:toml, {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})

  call dein#end()
  call dein#save_state()
endif

" 未インストールを確認
if dein#check_install()
  call dein#install()
endif

filetype plugin indent on
