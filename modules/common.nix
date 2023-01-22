# WARNING: This file is shared among system configuration such as macOS, nixOS which use
# home-manager **module**. Don't mess this with `homeConfiguration` because some
# of attributes don't exist in home-manager
{ self, inputs, config, pkgs, lib, options, ... }:

{
  imports = [ ./options.nix ./nixpkgs.nix ];

  ## Comment out this line if I want home-manager manage itself
  # hm = import ./shared;

  ## I don't use nix to manage shell configuration
  # programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Environment setup
  environment = {
    # install system-wide packages
    systemPackages = with pkgs; [
      # standard toolset
      coreutils
      curl
      jq
      wget
      # helpful shell stuff
      bat
      fzf
      less
      ripgrep
      zsh
      fish
    ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${inputs.nixpkgs}";
      stable.source = "${inputs.stable}";
    };
    ## List of acceptable shells in /etc/shells
    # shells = with pkgs; [ bash zsh fish ];
  };

}
