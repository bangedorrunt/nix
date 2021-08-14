{ config, lib, pkgs, home-manager, ... }:


let
  # REF: https://github.com/nix-community/home-manager/wiki/FAQ
  cfg = config.my.modules.dev;

in

{
  options = with lib; {
    my.modules.dev = {
      enable = mkEnableOption ''
        Whether to enable dev module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.hm.packages = with pkgs; [
        # Used system git for keychain integration
        # git 
        gh
        # clangd
        clang-tools
        # python with default packages
        (python39.withPackages (ps: with ps; [ pip pipx black ]))
        lua
        fennel
        nodejs
        nodePackages.pnpm
        nodePackages.yarn
        # lazygit
        nixfmt
        nixpkgs-fmt
        (callPackage ../pkgs/fnlfmt.nix { })
        shfmt
        treefmt
      ];
    };
}
