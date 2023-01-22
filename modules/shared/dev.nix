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
        ## use system git for keychain integration
        # git
        # go
        (callPackage ../pkgs/fnlfmt.nix { })
        clang-tools
        clojure-lsp
        emacs
        fnm
        gh
        lua
        neovim
        nixfmt
        nixpkgs-fmt
        nodePackages.bash-language-server
        nodePackages.dockerfile-language-server-nodejs
        nodePackages.eslint_d
        nodePackages.markdownlint-cli
        nodePackages.pnpm
        nodePackages.prettier
        nodePackages.stylelint
        nodePackages.typescript-language-server
        nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted # HTML, CSS, JSON LSPs
        nodePackages.yaml-language-server
        nodePackages.yarn
        rnix-lsp
        shellcheck
        shfmt
        stylua
        sumneko-lua-language-server
        tmux
        tree-sitter
        treefmt
        /* (callPackage ../pkgs/tailwind.nix { }) */
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
