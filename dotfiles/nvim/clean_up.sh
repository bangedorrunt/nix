#!/usr/bin/env bash
CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache/nvim}"
STATE_PATH="${XDG_STATE_HOME:-$HOME/.local/state/nvim}"
OLD_PATH="${XDG_DATA_HOME:-$HOME/.local/share/nvim/site}"

echo 'Removing old files ...'
rm -rf "$OLD_PATH/lua"
rm -rf "$CACHE_PATH/luacache"
rm -rf "$CACHE_PATH/log"
rm -rf "$CACHE_PATH/lsp.log"
rm -rf "$CACHE_PATH/packer.nvim.log"
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


echo 'Done!'
