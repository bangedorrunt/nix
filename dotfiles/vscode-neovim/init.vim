
" Set AuGroup
augroup VIMRC-GROUP
  autocmd!
  autocmd CursorHold *? syntax sync minlines=300
augroup END


augroup TYPESRC
  autocmd!
augroup END

" Custom command for source files
command! -nargs=1 ImportRCFrom
  \ execute 'source' fnamemodify(expand('<sfile>'), ':h').'/config/'.<args>.'.rc.vim'

command! -nargs=1 ImportPlugConfigFrom
  \ execute 'source' fnamemodify(expand('<sfile>'), ':h').'/plugins/'.<args>.'.vim'

command! -nargs=1 ImportLuaPlugConfigFrom
  \ execute 'luafile' fnamemodify(expand('<sfile>'), ':h').'/plugins/'.<args>.'.lua'

" Initialize base requirements
if has('vim_starting')
  ImportRCFrom 'base'
endif

" -----------
" PLUG MANAGER
" -----------

if empty(glob(expand('~/.config/nvim/autoload/plug.vim')))

  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC

endif

call plug#begin(expand('~/.cache/vim/plugged'))

" automatically install missing plugins on startup
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  autocmd VimEnter * PlugInstall | q
endif


" NAVIGATION PLUGINS
" ------------------
" Plug 'asvetliakov/vim-easymotion'

" UTILITY PLUGINS
" ---------------

Plug 'tpope/vim-repeat'

Plug 'haya14busa/is.vim'

Plug 'haya14busa/vim-asterisk'

" Plug 'brglng/vim-im-select'

" COMPLETION PLUGINS
" ------------------

call plug#end()

" Disable packpath
set packpath=

" ----------
" LOAD PLUGIN CONFIGS
" ----------
ImportRCFrom 'general'
ImportRCFrom 'keybindings'

" Reload vim config automatically
execute 'autocmd VIMRC-GROUP BufWritePost '.$NVIM_PATH.'/config/*,$MYVIMRC nested'
  \ .' source $MYVIMRC | redraw'

" Auto change input setting
" Ref: https://github.com/asvetliakov/vscode-neovim/issues/68
" Installed vim-imselect fixed issue
autocmd InsertLeave * :!/usr/local/bin/im-select com.apple.keylayout.ABC
