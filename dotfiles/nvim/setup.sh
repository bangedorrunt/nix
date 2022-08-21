#!/usr/bin/env bash
CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache/nvim}"
STATE_PATH="${XDG_STATE_HOME:-$HOME/.local/state/nvim}"
OLD_PATH="${XDG_DATA_HOME:-$HOME/.local/share/nvim/site}"
PACK_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start"
# ANISEED_PATH="${PACK_PATH}/aniseed"
IMPATIENT_PATH="${PACK_PATH}/impatient.nvim"
LUAFUN_PATH="${PACK_PATH}/luafun.nvim"
HOTPOT_PATH="${PACK_PATH}/hotpot.nvim"
PACKER_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt/packer.nvim"
# T9_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt/cmp-tabnine"

echo 'Removing old files ...'
rm -rf "$OLD_PATH/lua"
rm -rf "$OLD_PATH/pack"
rm -rf "$CACHE_PATH/packer.nvim"
rm -rf "$CACHE_PATH/packer_hererocks"
rm -rf "$CACHE_PATH/packer.nvim.log"
rm -rf "$CACHE_PATH/luacache_chunks"
rm -rf "$CACHE_PATH/luacache_modpaths"
rm -rf "$CACHE_PATH/luacache"
rm -rf "$CACHE_PATH/log"
rm -rf "$CACHE_PATH/lsp.log"
rm -rf "$STATE_PATH/log"
rm -rf "$STATE_PATH/lsp.log"

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

# echo 'Downloading Aniseed ...'

# if [ ! -d "$ANISEED_PATH" ]; then
#   git clone "https://github.com/Olical/aniseed.git" "$ANISEED_PATH"
# fi

# cd "$ANISEED_PATH" && git fetch && git checkout develop

echo 'Downloading Luafun ...'

if [ ! -d "$LUAFUN_PATH" ]; then
  git clone "https://github.com/babygau/luafun.nvim" "$LUAFUN_PATH"
fi
cd "$LUAFUN_PATH" && git fetch && git checkout neovim

echo 'Downloading Hotpot...'

if [ ! -d "$HOTPOT_PATH" ]; then
  git clone "https://github.com/rktjmp/hotpot.nvim.git" "$HOTPOT_PATH"
fi
cd "$HOTPOT_PATH" && git fetch && git checkout nightly

echo 'Downloading Packer ...'

if [ ! -d "$PACKER_PATH" ]; then
  git clone "https://github.com/wbthomason/packer.nvim" "$PACKER_PATH"
fi

echo 'Downloading impatient.nvim ...'

if [ ! -d "$IMPATIENT_PATH" ]; then
  git clone "https://github.com/lewis6991/impatient.nvim.git" "$IMPATIENT_PATH"
fi

if [ ! -e "${STATE_PATH}/swap" ]; then
  echo "Creating vim swap/backup/undo/view folders inside ${STATE_PATH}/nvim ..."
  mkdir -p "${STATE_PATH}/backup"
  mkdir -p "${STATE_PATH}/session"
  mkdir -p "${STATE_PATH}/swap"
  mkdir -p "${STATE_PATH}/tags"
  mkdir -p "${STATE_PATH}/undo"
  mkdir -p "${STATE_PATH}/view"
fi

echo 'Installing Packer plugins ...'
# A workaround for Packer commands are not available on first run
nvim --headless -c 'qa'
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

echo 'Sucessfully install Neovim config'

# echo 'Installing TabNine binary ...'

# if [ -e "$T9_PATH" ]; then
#   cd "$T9_PATH" && ./install.sh
# fi
