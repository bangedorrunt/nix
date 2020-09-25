
augroup VIMRC-GROUP
  au!
  au CursorHold *? syntax sync minlines=300
augroup END


augroup TYPESRC
  au!
augroup END

" Custom command for source files
command! -nargs=1 ImportRCFrom
  \ execute 'source' fnamemodify(expand('<sfile>'), ':h').'/config/'.<args>.'.vim'

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
  au VimEnter * PlugInstall --sync | source $MYVIMRC

endif

call plug#begin(expand('~/.cache/nvim/plugged'))

" Automatically install missing plugins on startup
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  au VimEnter * PlugInstall --sync | q
endif

" UI PLUGINS
" ----------

" Plug 'machakann/vim-highlightedyank'
" Plug 'chrisbra/Colorizer'
" Plug 'junegunn/vim-peekaboo'
" Plug 'yuttie/comfortable-motion.vim'
" Plug 'NLKNguyen/papercolor-theme'
" Plug 'reedes/vim-colors-pencil'
" Plug 'srcery-colors/srcery-vim'
" Plug 'sonph/onehalf', {'rtp': '/vim'}
Plug 'Yggdroot/indentLine'
Plug 'ap/vim-buftabline'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/vim-easy-align'
Plug 'mhinz/vim-startify'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'ntpeters/vim-better-whitespace'
Plug 'psliwka/vim-smoothie'
Plug 'cocopon/pgmnt.vim'
Plug 'cocopon/iceberg.vim'
" Plug 'sainnhe/sonokai'
Plug 'babygau/sonokai', { 'branch': 'custom' }
Plug 't9md/vim-quickhl'
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'lambdalisue/fern.vim'
Plug 'kassio/neoterm'

" NAVIGATION PLUGINS
" ------------------

Plug 'junegunn/fzf', {
          \'dir': '~/.fzf',
          \'do': './install --all'
          \}

Plug 'junegunn/fzf.vim'
" Plug 'mhinz/vim-grepper'
" Plug 'asvetliakov/vim-easymotion'
" Plug 'yuki-ycino/fzf-preview.vim'

" UTILITY PLUGINS
" ---------------

" Plug 'alok/notational-fzf-vim'
" Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
" Plug 'ihsanturk/neuron.vim', { 'branch': 'dev' }
" Plug 'rlue/vim-barbaric'
" Plug 'tpope/vim-unimpaired'
" Plug 'christoomey/vim-tmux-navigator'
" Plug 'wellle/context.vim'
" Plug 'wellle/tmux-complete.vim'
Plug 'airblade/vim-rooter'
Plug 'brglng/vim-im-select'
Plug 'haya14busa/is.vim'
Plug 'haya14busa/vim-asterisk'
Plug 'rhysd/accelerated-jk'
Plug 'romainl/vim-qf'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vimwiki/vimwiki'
Plug 'michal-h21/vim-zettel'

" COMPLETION PLUGINS
" ------------------
" Plug 'aca/neuron-language-server'
Plug 'neovim/nvim-lsp'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-treesitter/completion-treesitter'
Plug 'haorenW1025/completion-nvim'
Plug 'nathunsmitty/diagnostic-nvim'
Plug 'steelsojka/completion-buffers'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }

" LANGUAGE PLUGINS
" ----------------

" Markdown

" Removed due to conflict with vimwiki
" Plug 'SidOfc/mkdx', { 'for': ['markdown', 'md'] }

" Plug 'iamcco/markdown-preview.nvim', {
"           \'for': ['markdown', 'md', 'rmd', 'pandoc.markdown'],
"           \'do': 'cd app & yarn install'
"           \}

" VimL
Plug 'vim-jp/syntax-vim-ex', { 'for': ['vim'] }

" Tmux
" Plug 'tmux-plugins/vim-tmux'

" Context
Plug 'Shougo/context_filetype.vim'

call plug#end()

" Disable packpath
set packpath=

" ----------
" LOAD PLUGIN CONFIGS
" ----------

" ImportRCFrom 'status-line'
ImportRCFrom 'filetypes'
ImportRCFrom 'general'
ImportRCFrom 'keybindings'
ImportRCFrom 'space'
ImportRCFrom 'theme'

" ImportPlugConfigFrom 'markdown'
ImportPlugConfigFrom    'completion-nvim'
ImportLuaPlugConfigFrom 'colorizer'
ImportLuaPlugConfigFrom 'nvim-lsp'
ImportPlugConfigFrom    'asterisk'
ImportPlugConfigFrom    'betterwhitespace'
ImportPlugConfigFrom    'fzf'
ImportPlugConfigFrom    'lightline'
ImportPlugConfigFrom    'prettier'
ImportPlugConfigFrom    'startify'
ImportPlugConfigFrom    'vim-rooter'
ImportPlugConfigFrom    'fern'
ImportPlugConfigFrom    'neoterm'
ImportPlugConfigFrom    'im-select'
ImportPlugConfigFrom    'zettel'

" Reload vim config automatically
execute 'au VIMRC-GROUP BufWritePost '.$NVIM_PATH.'/config/*,$MYVIMRC nested'
  \ .' source $MYVIMRC | redraw'

