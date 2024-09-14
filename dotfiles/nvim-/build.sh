#!/bin/bash

cd ~/workspace/neovim || exit

git pull

sudo make distclean

make CMAKE_BUILD_TYPE=Release

sudo make install

echo 'Build successfully'
