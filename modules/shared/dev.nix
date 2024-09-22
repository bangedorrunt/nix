{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  # REF: https://github.com/nix-community/home-manager/wiki/FAQ
  cfg = config.tdt.modules.shared.pkgs.dev;
in {
  options = with lib; {
    tdt.modules.shared.pkgs.dev = {
      enable = mkEnableOption ''
        Whether to enable dev module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      tdt.hm.packages = with pkgs; [
        # clangd
        # clojure-lsp
        # emacs
        # go
        # nodePackages_latest.prettier
        alejandra
        bash-language-server
        clang-tools
        diagnostic-languageserver
        dockerfile-language-server-nodejs
        eslint_d
        fnlfmt
        gh
        git
        lazygit
        lua
        lua-language-server
        luajitPackages.fennel
        luajitPackages.luarocks
        markdownlint-cli
        marksman
        neovim
        nixd
        pnpm
        qmk
        shellcheck
        shfmt
        stylelint
        stylua
        tailwindcss
        tmux
        tree-sitter
        treefmt
        typescript-language-server
        vscode-langservers-extracted # HTML, CSS, JSON LSPs
        yaml-language-server
        yarn
        zellij
      ];
    };
}
