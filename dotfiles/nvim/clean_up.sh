#!/usr/bin/env bash
DATA_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim"
CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache}/nvim"
STATE_PATH="${XDG_STATE_HOME:-$HOME/.local/state}/nvim"
OLD_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site"

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
rm -rf "$STATE_PATH/lazy"
rm -rf "$STATE_PATH/log"
rm -rf "$STATE_PATH/lsp.log"
rm -rf "$STATE_PATH/mason.log"
rm -rf "$STATE_PATH/noice.log"

if [ -d "./lua" ]; then
  rm -rf "./lua"
  echo "Removing lua folder ..."
fi

if [ -d "$CACHE_PATH/lazy" ]; then
  rm -rf "$CACHE_PATH/lazy"
  echo "Removing lazy folder ..."
fi
if [ -d "$CACHE_PATH/hotpot" ]; then
  rm -rf "$CACHE_PATH/hotpot"
  rm -rf "$CACHE_PATH/hotpot.log"
  echo "Removing Hotpot folder ..."
fi

echo 'Done!'
