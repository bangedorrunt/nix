
#! /usr/bin/env bash

echo 'Remove old packages but current one'

nix-env --delete-generations old

echo 'Run garbage collector'
nix-store --gc --print-dead

echo 'Clean up from all users'
nix-collect-garbage -d
