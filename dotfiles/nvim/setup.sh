#!/usr/bin/env bash
CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache/nvim}"
OLD_DIR="${XDG_DATA_HOME:-$HOME/.local/share/nvim/site}"
PACK_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start"
ANISEED_DIR="${PACK_PATH}/aniseed"
PACKER_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt/packer.nvim"

echo 'Removing old files ...'
rm -rf "$OLD_DIR/lua"
rm -rf "$OLD_DIR/pack"
rm -rf "$CACHE_DIR/log"
rm -rf "$CACHE_DIR/lsp.log"
rm -rf "$CACHE_DIR/packer.nvim.log"

if [ -d "./lua" ]; then
	rm -rf "./lua"
	echo "Removing lua folder ..."
fi

mkdir -p "$PACK_PATH"

echo 'Downloading Aniseed ...'

if [ ! -d "$ANISEED_DIR" ]; then
	git clone "https://github.com/Olical/aniseed.git" "$ANISEED_DIR"
fi

cd "$ANISEED_DIR" && git fetch && git checkout develop

echo 'Downloading Packer ...'

if [ ! -d "$PACKER_DIR" ]; then
	git clone "https://github.com/wbthomason/packer.nvim" "$PACKER_DIR"
fi

if [ ! -e "${CACHE_DIR}/swap" ]; then
	echo "Creating vim swap/backup/undo/view folders inside ${CACHE_DIR}/nvim ..."
	mkdir -p "${CACHE_DIR}/backup"
	mkdir -p "${CACHE_DIR}/session"
	mkdir -p "${CACHE_DIR}/swap"
	mkdir -p "${CACHE_DIR}/tags"
	mkdir -p "${CACHE_DIR}/undo"
	mkdir -p "${CACHE_DIR}/view"
fi

echo 'Installing Packer plugins ...'

nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

echo 'Sucessfully install Neovim config'
