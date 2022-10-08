#!/bin/bash

# Run this after upgrade Yabai

# Calculate new hash for the binary
SHA256=$(shasum -a 256 /usr/local/bin/yabai | cut -d' ' -f 1)
echo "New yabai hash: $SHA256"

# Replace the hash in-place in the sudoers file
sudo sed -i '' -e 's/sha256:[[:alnum:]]*/sha256:'${SHA256}'/' /private/etc/sudoers.d/yabai
