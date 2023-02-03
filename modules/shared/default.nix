# WARNING: This file is shared among system configuration such as macOS, nixOS which use
# home-manager **module**. Don't mess this with `homeConfiguration` because some
# of attributes don't exist in home-manager
{
  self,
  inputs,
  config,
  pkgs,
  lib,
  options,
  ...
}:
with lib.mine; let
  shared = rakeLeaves ./.;
in {
  imports = [
    shared.options
    shared.user
    shared.hm
    shared.nixpkgs
    shared.pkgs
    shared.dev
  ];
  # Environment setup
  environment = {
    # System-wide packages
    systemPackages = with pkgs; [
      # standard toolset
      coreutils
      curl
      jq
      wget
      # helpful shell stuff
      bat
      fish
      fzf
      less
      ripgrep
      zsh
    ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${inputs.nixpkgs}";
      stable.source = "${inputs.stable}";
    };
    # List of acceptable shells in /etc/shells
    shells = with pkgs; [bash zsh fish];
  };
}
