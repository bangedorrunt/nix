{ config, lib, pkgs, ... }: {
  homebrew = {
    casks = [
      "basictex"
      "betterdiscord-installer"
      "bitwarden"
      "discord"
      "discord-ptb"
      "element-nightly"
      "firefox-developer-edition"
      "git-credential-manager-core"
      "iina"
      "keycastr"
      "maccy"
      "microsoft-auto-update"
      "microsoft-edge-beta"
      "openkey"
      "visual-studio-code-insiders"
      "wezterm-nightly"
    ];
  };
}
