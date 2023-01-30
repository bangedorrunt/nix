{
  config,
  inputs,
  lib,
  pkgs,
  options,
  ...
}: {
  imports = [./dotfiles.nix ./dev.nix];

  my.hm.packages = with pkgs; [
    # cmake
    # libgccjit
    # openssh
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
    gcc
    ghc
    gnugrep
    gnupg
    gnused
    htop
    httpie
    hyperfine
    jq
    just
    kepubify
    neofetch
    pandoc
    ripgrep
    rsync
    tmux
    zoxide
  ];
}
