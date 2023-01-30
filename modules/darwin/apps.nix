{
  config,
  lib,
  pkgs,
  ...
}: {
  homebrew = {
    casks = [
      "basictex"
      "betterdiscord-installer"
      "bitwarden"
      "discord"
      "element-nightly"
      "firefox-developer-edition"
      "git-credential-manager-core"
      "iina"
      "keycastr"
      "maccy"
      "microsoft-edge-beta"
      "neovide"
      "openkey"
      "visual-studio-code-insiders"
      "wezterm-nightly"
    ];
  };
}
