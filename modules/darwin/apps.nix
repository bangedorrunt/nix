{ config, lib, pkgs, ... }: {
  homebrew = {
    casks = [
      "firefox-beta"
      "hammerspoon"
      "iterm2"
      "iina"
      "karabiner-elements"
      "keepingyouawake"
      "maccy"
      "syncthing"
      "visual-studio-code-insiders"
      "element"
      "bitwarden"
      "discord"
    ];
  };
}
