{ config, lib, pkgs, home-manager, ... }:


let
  # REF: https://github.com/nix-community/home-manager/wiki/FAQ
  cfg = config.my.modules.neovim;
  # home = config.my.user.home;

in

{
  options = with lib; {
    my.modules.neovim = {
      enable = mkEnableOption ''
        Whether to enable Neovim module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.hm.packages = with pkgs; [
        neovim-nightly
        tree-sitter
        universal-ctags
        glow
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
        nodePackages.vscode-css-languageserver-bin
        nodePackages.vscode-html-languageserver-bin
        nodePackages.vscode-json-languageserver

        (callPackage ../pkgs/tailwind.nix {})
        (
          writeScriptBin "tailwind-lsp" ''
            #!/usr/bin/env sh
            node ${(callPackage ../pkgs/tailwind.nix {})}/share/vscode/extensions/bradlc.vscode-tailwindcss/dist/server/index.js --stdio
          ''
        )
        shellcheck
        rnix-lsp
        stylua
        clojure-lsp
        (if pkgs.stdenv.isDarwin then pkgs.sumneko-lua-language-server-macos else pkgs.sumneko-lua-language-server)
      ];
    };
}
