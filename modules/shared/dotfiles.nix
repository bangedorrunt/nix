{ config, lib, pkgs, ... }:
with lib;
let

  # Manage files with symlink
  # Stole from: https://github.com/nix-community/home-manager/blob/master/modules/files.nix#L64
  mkOutOfStoreSymlink = path:
    let
      pathStr = toString path;
      name = lib.hm.strings.storeFileName (baseNameOf pathStr);
    in
      pkgs.runCommandLocal name {} ''ln -s ${escapeShellArg pathStr} $out'';

  cfg = config.my.modules.dotfiles;
in

{
  options = with lib; {
    my.modules.dotfiles = {
      enable = mkEnableOption ''
        Whether to enable dotfiles module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.hm = {
        file = {
          ".zshrc".source = ../../dotfiles/.zshrc;
          ".tmux.conf".source = ../../dotfiles/tmux/.tmux.conf;
          ".skhdrc".source = ../../dotfiles/skhd/skhdrc;
        };

        configFile = {
          "nixpkgs/config.nix".source = ../config.nix;
          "alacritty".source = ../../dotfiles/alacritty;
          "bat".source = ../../dotfiles/bat;
          "emacs".source = ../../dotfiles/pure-emacs;
          # "nvim".source = ../../dotfiles/nvim;
          "karabiner/karabiner.edn".source = ../../dotfiles/karabiner/karabiner.edn;
          "skhd".source = ../../dotfiles/skhd;
          "tmux".source = ../../dotfiles/tmux;
          "yabai".source = ../../dotfiles/yabai;
        };
      };
    };
}
