{
  config,
  inputs,
  lib,
  pkgs,
  options,
  ...
}: {
  tdt.hm.packages = with pkgs; [
    # cmake
    # libgccjit
    # openssh
    bat
    cachix
    curl
    curlie
    delta
    direnv
    eza
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
