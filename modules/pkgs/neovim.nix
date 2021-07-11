{ config, pkgs, home-manager, ... }:

{
  home.packages = with pkgs; [
    clang-tools # clangd
    neovim-nightly
    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
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
    (callPackage ../pkgs/tailwind.nix { })
    (writeScriptBin "tailwind-lsp" ''
      #!/usr/bin/env sh
      node ${(callPackage ../pkgs/tailwind.nix { })}/share/vscode/extensions/bradlc.vscode-tailwindcss/dist/server/index.js --stdio
    '')
    shellcheck
    rnix-lsp
    stylua
    clojure-lsp
    sumneko-lua-language-server
  ];
}
