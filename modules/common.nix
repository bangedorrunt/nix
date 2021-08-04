# WARNING: This file is shared among OS such as macOS, nixOS which use
# home-manager **module**. Don't mess this with `homeConfiguration` because some
# of attributes don't exist in home-manager
{ inputs, config, pkgs, lib, options, ... }:

{
  imports = [ ./options.nix ./nixpkgs.nix ./overlays.nix ];
  programs.zsh = {
    enable = true;
  };

  # Environment setup
  environment = {
    # Install system-wide packages
    systemPackages = with pkgs; [
      # Editors
      neovim-nightly
      # Standard toolset
      coreutils
      curl
      jq
      wget
      # Helpful shell stuff
      bat
      fzf
      less
      ripgrep
      zsh
      # Languages
      python3
    ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${inputs.nixpkgs}";
    };
    # List of acceptable shells in /etc/shells
    shells = with pkgs; [ bash zsh fish ];
  };

}
