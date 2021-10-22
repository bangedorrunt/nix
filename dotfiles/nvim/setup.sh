#!/usr/bin/env bash
CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache/nvim}"
OLD_PATH="${XDG_DATA_HOME:-$HOME/.local/share/nvim/site}"
PACK_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start"
ANISEED_PATH="${PACK_PATH}/aniseed"
IMPATIENT_PATH="${PACK_PATH}/impatient.nvim"
FILETYPE_NVIM_PATH="${PACK_PATH}/filetype.nvim"
# HOTPOT_PATH="${PACK_PATH}/hotpot.nvim"
PACKER_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt/packer.nvim"
T9_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt/cmp-tabnine"

echo 'Removing old files ...'
rm -rf "$OLD_PATH/lua"
rm -rf "$OLD_PATH/pack"
rm -rf "$CACHE_PATH/luacache"
rm -rf "$CACHE_PATH/log"
rm -rf "$CACHE_PATH/lsp.log"
rm -rf "$CACHE_PATH/packer.nvim.log"

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

echo 'Downloading Aniseed ...'

if [ ! -d "$ANISEED_PATH" ]; then
  git clone "https://github.com/Olical/aniseed.git" "$ANISEED_PATH"
fi

cd "$ANISEED_PATH" && git fetch && git checkout develop

# echo 'Downloading Hotpot...'

# if [ ! -d "$HOTPOT_PATH" ]; then
#   git clone "https://github.com/rktjmp/hotpot.nvim.git" "$HOTPOT_PATH"
# fi

echo 'Downloading Packer ...'

if [ ! -d "$PACKER_PATH" ]; then
  git clone "https://github.com/wbthomason/packer.nvim" "$PACKER_PATH"
fi

echo 'Downloading impatient.nvim ...'

if [ ! -d "$IMPATIENT_PATH" ]; then
  git clone "https://github.com/lewis6991/impatient.nvim.git" "$IMPATIENT_PATH"
fi

echo 'Downloading filetype.nvim ...'

if [ ! -d "$FILETYPE_NVIM_PATH" ]; then
  git clone "https://github.com/nathom/filetype.nvim" "$FILETYPE_NVIM_PATH"
fi

if [ ! -e "${CACHE_PATH}/swap" ]; then
  echo "Creating vim swap/backup/undo/view folders inside ${CACHE_PATH}/nvim ..."
  mkdir -p "${CACHE_PATH}/backup"
  mkdir -p "${CACHE_PATH}/session"
  mkdir -p "${CACHE_PATH}/swap"
  mkdir -p "${CACHE_PATH}/tags"
  mkdir -p "${CACHE_PATH}/undo"
  mkdir -p "${CACHE_PATH}/view"
fi

echo 'Installing Packer plugins ...'

NVIM_SKIP_PLUGIN_CONFIGS=1 nvim --headless -c 'autocmd User PackerComplete qa' -c 'PackerSync'

echo 'Sucessfully install Neovim config'

echo 'Installing TabNine binary ...'
cd "$T9_PATH" && ./install.sh
