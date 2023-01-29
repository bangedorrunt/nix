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
        # clangd
        # emacs
        # go
        ## use system git for keychain integration
        asdf-vm
        clang-tools
        clojure-lsp
        fnlfmt
        fnm
        gh
        git
        lua
        marksman
        neovim
        nil
        nixpkgs-fmt
        nodePackages.bash-language-server
        nodePackages.diagnostic-languageserver
        nodePackages.dockerfile-language-server-nodejs
        nodePackages.eslint_d
        nodePackages.grammarly-languageserver
        nodePackages.markdownlint-cli
        nodePackages.pnpm
        nodePackages.prettier
        nodePackages.stylelint
        nodePackages.typescript-language-server
        nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted # HTML, CSS, JSON LSPs
        nodePackages.yaml-language-server
        nodePackages.yarn
        shellcheck
        shfmt
        stylua
        sumneko-lua-language-server
        tmux
        tree-sitter
        treefmt
        vscode-extensions.bradlc.vscode-tailwindcss
        (
          writeScriptBin "tailwind-lsp" ''
            #!/usr/bin/env sh
            node ${vscode-extensions.bradlc.vscode-tailwindcss}/share/vscode/extensions/bradlc.vscode-tailwindcss/dist/server/index.js --stdio
          ''
        )
      ];
    };
}
