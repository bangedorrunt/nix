{ inputs, config, pkgs, lib, options, ... }:

{
  imports = [ ./primary.nix ./nixpkgs.nix ./overlays.nix ];

  programs.zsh = {
    enable = true;
  };

  # Environment setup
  environment = {
    systemPackages = with pkgs; [
      # Editors
      neovim-nightly
      # emacsGcc

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
