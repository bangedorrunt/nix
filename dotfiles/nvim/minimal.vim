" -----------
" plug manager
" -----------

if empty(glob(expand('~/.config/nvim/autoload/plug.vim')))

  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC

endif

call plug#begin(expand('~/.cache/nvim/plugged'))

" automatically install missing plugins on startup
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  autocmd VimEnter * PlugInstall --sync | q
endif

" ui plugins
" ----------

" Plug 'chrisbra/Colorizer'
" Plug 'junegunn/vim-peekaboo'
" Plug 'yuttie/comfortable-motion.vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'Yggdroot/indentLine'
Plug 'ap/vim-buftabline'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/vim-easy-align'
Plug 'machakann/vim-highlightedyank'
Plug 'mhinz/vim-startify'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'ntpeters/vim-better-whitespace'
Plug 'psliwka/vim-smoothie'
Plug 'reedes/vim-colors-pencil'
Plug 'srcery-colors/srcery-vim'
Plug 'sonph/onehalf', {'rtp': '/vim'}
Plug 't9md/vim-quickhl'

" navigation plugins
" ------------------

Plug 'junegunn/fzf', {
          \'dir': '~/.fzf',
          \'do': './install --all'
          \}

Plug 'junegunn/fzf.vim'
" Plug 'mhinz/vim-grepper'
" Plug 'asvetliakov/vim-easymotion'
" Plug 'yuki-ycino/fzf-preview.vim'

" utility plugins
" ---------------

" Plug 'alok/notational-fzf-vim'
" Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
" Plug 'ihsanturk/neuron.vim'
" Plug 'rlue/vim-barbaric'
" Plug 'tpope/vim-unimpaired'
Plug 'airblade/vim-rooter'
Plug 'brglng/vim-im-select'
Plug 'christoomey/vim-tmux-navigator'
Plug 'haya14busa/is.vim'
Plug 'haya14busa/vim-asterisk'
Plug 'michal-h21/vim-zettel'
Plug 'rhysd/accelerated-jk'
Plug 'romainl/vim-qf'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vimwiki/vimwiki'
Plug 'wellle/context.vim'
Plug 'wellle/tmux-complete.vim'

" completion plugins
" ------------------
" Plug 'aca/neuron-language-server'
Plug 'haorenW1025/completion-nvim'
Plug 'neovim/nvim-lsp'
Plug 'nvim-treesitter/completion-treesitter'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'steelsojka/completion-buffers'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }

" language plugins
" ----------------
" markdown

" removed due to conflict with vimwiki
" Plug 'SidOfc/mkdx', { 'for': ['markdown', 'md'] }

" Plug 'iamcco/markdown-preview.nvim', {
"           \'for': ['markdown', 'md', 'rmd', 'pandoc.markdown'],
"           \'do': 'cd app & yarn install'
"           \}

" viml
Plug 'vim-jp/syntax-vim-ex', { 'for': ['vim'] }

" tmux
Plug 'tmux-plugins/vim-tmux'

" context
Plug 'Shougo/context_filetype.vim'

call plug#end()

" disable packpath
set packpath=
