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
        tree-sitter
        clang-tools
        lua
        nodejs
        nodePackages.pnpm
        nodePackages.yarn
        (callPackage ../pkgs/fnlfmt.nix { })
        shfmt
        clojure-lsp
        sumneko-lua-language-server
        nodePackages.bash-language-server
        nodePackages.dockerfile-language-server-nodejs
        nodePackages.yaml-language-server
        nodePackages.eslint_d
        nodePackages.markdownlint-cli
        nodePackages.pyright
        nodePackages.prettier
        nodePackages.stylelint
        nodePackages.typescript-language-server
        nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted # HTML, CSS, JSON LSPs
        (callPackage ../pkgs/tailwind.nix { })
        (
          writeScriptBin "tailwind-lsp" ''
            #!/usr/bin/env sh
            node ${(callPackage ../pkgs/tailwind.nix {})}/share/vscode/extensions/bradlc.vscode-tailwindcss/dist/server/index.js --stdio
          ''
        )
        shellcheck
        rnix-lsp
        nixfmt
        nixpkgs-fmt
        treefmt
        stylua
      ];
    };
}
