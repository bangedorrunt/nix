{ config, lib, pkgs, home-manager, ... }:
let
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
          "nvim".source = ../../dotfiles/nvim;
          "karabiner/karabiner.edn".source = ../../dotfiles/karabiner/karabiner.edn;
          "skhd".source = ../../dotfiles/skhd;
          "tmux".source = ../../dotfiles/tmux;
          "yabai".source = ../../dotfiles/yabai;
        };
      };
    };
}
