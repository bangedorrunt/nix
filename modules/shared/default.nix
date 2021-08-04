{ config, inputs, lib, pkgs, options, ... }:

{
  imports = [
    ./dotfiles.nix
    ./dev.nix
    ./neovim.nix
  ];

  my.modules = {
    dotfiles.enable = true;
    dev.enable = true;
    neovim.enable = true;
  };

  my.hm.packages = with pkgs; [
    bat
    cachix
    curl
    curlie
    delta
    direnv
    exa
    fd
    fzf
    fzy
    gawk
    ghc
    gnugrep
    gnupg
    gnused
    htop
    httpie
    hyperfine
    jq
    neofetch
    nixUnstable
    openssh
    pandoc
    ripgrep
    rsync
    tmux
    zoxide
  ];
}
