#!/usr/bin/env bash
DATA_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim"
CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache}/nvim"
STATE_PATH="${XDG_STATE_HOME:-$HOME/.local/state}/nvim"
OLD_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site"
PACK_START="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start"
PACK_OPT="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt"

echo 'Removing old files ...'
rm -rf "$OLD_PATH/lua"
rm -rf "$OLD_PATH/pack"
rm -rf "$CACHE_PATH/packer.nvim"
rm -rf "$CACHE_PATH/packer_hererocks"
rm -rf "$CACHE_PATH/packer.nvim.log"
rm -rf "$CACHE_PATH/luacache_chunks"
rm -rf "$CACHE_PATH/luacache_modpaths"
rm -rf "$CACHE_PATH/log"
rm -rf "$CACHE_PATH/lsp.log"
rm -rf "$CACHE_PATH/null-ls.log"
rm -rf "$CACHE_PATH/diffview.log"
rm -rf "$DATA_PATH/neorg.log"
rm -rf "$STATE_PATH/log"
rm -rf "$STATE_PATH/lsp.log"
rm -rf "$STATE_PATH/mason.log"
rm -rf "$STATE_PATH/noice.log"

if [ -d "./lua" ]; then
  rm -rf "./lua"
  echo "Removing lua folder ..."
fi

if [ -d "./plugin" ]; then
  rm -rf "./plugin"
  echo "Removing packer compiled folder ..."
fi

if [ -d "$CACHE_PATH/hotpot" ]; then
  rm -rf "$CACHE_PATH/hotpot"
  rm -rf "$CACHE_PATH/hotpot.log"
  echo "Removing Hotpot folder ..."
fi

echo 'Installing Packer plugins ...'

if [ ! -d "PACK_START" ]; then
  mkdir -p "$PACK_START"
  cd "$PACK_START" || exit
  git clone --depth 1 "https://github.com/rktjmp/hotpot.nvim"
  git clone -b main "https://github.com/wbthomason/packer.nvim"
  cd packer.nvim && git checkout 5cb06da03cccc7ec9f21405df98abf242c9b7b32 && cd ..
  git clone --depth 1 "https://github.com/nvim-lua/plenary.nvim"
  git clone --depth 1 "https://github.com/nvim-treesitter/nvim-treesitter"
  git clone --depth 1 "https://github.com/nvim-tree/nvim-web-devicons"
  git clone --depth 1 "https://github.com/Olical/conjure"
  git clone --depth 1 "https://github.com/gpanders/editorconfig.nvim"
  git clone --depth 1 "https://github.com/catppuccin/nvim"
fi

if [ ! -d "PACK_OPT" ]; then
  mkdir -p "$PACK_OPT"
  cd "$PACK_OPT" || exit
  git clone --depth 1 "https://github.com/echasnovski/mini.tabline"
  git clone --depth 1 "https://github.com/nvim-lualine/lualine.nvim"
  git clone --depth 1 "https://github.com/nvim-tree/nvim-tree.lua"
  git clone --depth 1 "https://github.com/echasnovski/mini.indentscope"
  git clone --depth 1 "https://github.com/ii14/autosplit.nvim"
  git clone --depth 1 "https://github.com/tpope/vim-eunuch"
  git clone --depth 1 "https://github.com/tpope/vim-repeat"
  git clone --depth 1 "https://github.com/linty-org/readline.nvim"
  git clone --depth 1 "https://github.com/p00f/nvim-ts-rainbow"
  git clone --depth 1 "https://github.com/andymass/vim-matchup"
  git clone --depth 1 "https://github.com/JoosepAlviste/nvim-ts-context-commentstring"
  git clone --depth 1 "https://github.com/echasnovski/mini.ai"
  git clone --depth 1 "https://github.com/echasnovski/mini.surround"
  git clone --depth 1 "https://github.com/echasnovski/mini.pairs"
  git clone --depth 1 "https://github.com/echasnovski/mini.bufremove"
  git clone --depth 1 "https://github.com/echasnovski/mini.align"
  git clone --depth 1 "https://github.com/echasnovski/mini.comment"
  git clone --depth 1 "https://github.com/NvChad/nvim-colorizer.lua"
  git clone --depth 1 "https://github.com/nvim-telescope/telescope-fzf-native.nvim"
  cd telescope-fzf-native.nvim && make && cd ..
  git clone --depth 1 "https://github.com/nvim-telescope/telescope-live-grep-args.nvim"
  git clone --depth 1 "https://github.com/nvim-telescope/telescope.nvim"
  git clone --depth 1 "https://github.com/hrsh7th/nvim-cmp"
  git clone --depth 1 "https://github.com/hrsh7th/cmp-cmdline"
  git clone --depth 1 "https://github.com/hrsh7th/cmp-path"
  git clone --depth 1 "https://github.com/hrsh7th/cmp-buffer"
  git clone --depth 1 "https://github.com/hrsh7th/cmp-calc"
  git clone --depth 1 "https://github.com/hrsh7th/cmp-nvim-lsp"
  git clone --depth 1 "https://github.com/hrsh7th/cmp-nvim-lua"
  git clone --depth 1 "https://github.com/PaterJason/cmp-conjure"
  git clone --depth 1 "https://github.com/L3MON4D3/LuaSnip"
  git clone --depth 1 "https://github.com/saadparwaiz1/cmp_luasnip"
  git clone --depth 1 "https://github.com/tpope/vim-fugitive"
  git clone --depth 1 "https://github.com/lewis6991/gitsigns.nvim"
  git clone --depth 1 "https://github.com/jaawerth/fennel.vim"
  git clone --depth 1 "https://github.com/folke/neodev.nvim"
  git clone --depth 1 "https://github.com/simrat39/rust-tools.nvim"
  git clone --depth 1 "https://github.com/nvim-neorg/neorg-telescope"
  git clone --depth 1 "https://github.com/nvim-neorg/neorg"
  git clone --depth 1 "https://github.com/williamboman/mason.nvim"
  git clone --depth 1 "https://github.com/jose-elias-alvarez/null-ls.nvim"
  git clone --depth 1 "https://github.com/neovim/nvim-lspconfig"
  git clone --depth 1 "https://github.com/ericpruitt/tmux.vim"
  git clone --depth 1 "https://github.com/aserowy/tmux.nvim"
fi

if [ ! -e "$STATE_PATH/swap" ]; then
  echo "Creating vim swap/backup/undo/view folders inside $STATE_PATH/nvim ..."
  mkdir -p "$STATE_PATH/backup"
  mkdir -p "$STATE_PATH/sessions"
  mkdir -p "$STATE_PATH/swap"
  mkdir -p "$STATE_PATH/tags"
  mkdir -p "$STATE_PATH/undo"
  mkdir -p "$STATE_PATH/view"
fi

# A workaround for Packer commands are not available on first run
nvim --headless +'qa'
# nvim --headless +'autocmd User PackerComplete quitall' +PackerSync

echo 'Sucessfully install Neovim config'
