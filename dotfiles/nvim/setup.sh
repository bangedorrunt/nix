#!/usr/bin/env bash
CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}"
ANISEED_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start"
PACKER_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/opt/packer.nvim"

mkdir -p $ANISEED_DIR

echo 'Downloading Aniseed ...'

if [ ! -d "$ANISEED_DIR/aniseed" ]; then
	git clone https://github.com/Olical/aniseed.git $ANISEED_DIR/aniseed
fi

cd $ANISEED_DIR/aniseed && git fetch && git checkout develop

echo 'Downloading Packer ...'
if [ ! -d "$PACKER_DIR" ]; then
	git clone "https://github.com/wbthomason/packer.nvim" "$PACKER_DIR"
fi

if [ ! -e "${CACHE_DIR}/nvim/swap" ]; then
	echo "Creating vim swap/backup/undo/view folders inside ${CACHE_DIR}/nvim ..."
	mkdir -p ${CACHE_DIR}/nvim/{backup,session,swap,tags,undo,view}
fi

echo 'Installing Packer plugins ...'

nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

echo '\nSucessfully install Neovim config'
