#!/usr/bin/env bash
EMACS_APP="/Applications/Emacs.app"

function uninstall_emacs {
  if [ -L "$EMACS_APP" ]; then
    echo 'Uninstalling old version...'
    rm -rf "$EMACS_APP"
    brew uninstall emacs-plus@30
  fi
}

function build_emacs_for_macos {
  echo 'Building Emacs Nightly...'
  brew install emacs-plus@30 --with-no-frame-refocus --with-xwidgets --with-imagemagick --with-native-comp --with-poll --with-modern-doom3-icon || return 0
  ln -s /usr/local/opt/emacs-plus@30/Emacs.app /Applications
  echo "Build Successfully!"
}

uninstall_emacs
build_emacs_for_macos
