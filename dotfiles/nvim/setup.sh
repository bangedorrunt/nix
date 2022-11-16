#!/usr/bin/env bash
DATA_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim"
CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache}/nvim"
STATE_PATH="${XDG_STATE_HOME:-$HOME/.local/state}/nvim"
OLD_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site"
PACK_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start"
HOTPOT_PATH="${PACK_PATH}/hotpot.nvim"
PACKER_PATH="${PACK_PATH}/packer.nvim"

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

if [ -d "$CACHE_PATH/hotpot" ]; then
  rm -rf "$CACHE_PATH/hotpot"
  rm -rf "$CACHE_PATH/hotpot.log"
  echo "Removing Hotpot folder ..."
fi

mkdir -p "$PACK_PATH"

echo 'Downloading Hotpot...'

if [ ! -d "$HOTPOT_PATH" ]; then
  git clone --depth 1 "https://github.com/rktjmp/hotpot.nvim" "$HOTPOT_PATH"
fi

echo 'Downloading Packer ...'

if [ ! -d "$PACKER_PATH" ]; then
  git clone --depth 1 "https://github.com/wbthomason/packer.nvim" "$PACKER_PATH"
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

echo 'Installing Packer plugins ...'
# A workaround for Packer commands are not available on first run
nvim --headless +'qa'
nvim --headless +'sleep 1' +'autocmd User PackerComplete quitall' +PackerSync

echo 'Sucessfully install Neovim config'
