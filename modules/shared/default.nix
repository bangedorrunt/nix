{ config, lib, pkgs, ... }:

{
  imports = [
    ./dotfiles.nix
    ./neovim.nix
  ];

  my.modules = {
    dotfiles.enable = true;
    neovim.enable = true;
  };

  my.hm.packages = with pkgs; [
    # python with default packages
    (python39.withPackages (ps: with ps; [ black ]))
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
    lazygit
    neuron
    neofetch
    nixUnstable
    nixfmt
    nixpkgs-fmt
    nodejs_latest
    openssh
    pandoc
    ripgrep
    rsync
    tmux
    yarn
    zoxide
  ];
}
