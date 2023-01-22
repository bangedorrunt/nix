{ config, inputs, lib, pkgs, options, ... }: {
  imports = [ ./dotfiles.nix ./dev.nix ];

  my.hm.packages = with pkgs; [
    # openssh
    bat
    # cmake
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
    kepubify
    # libgccjit
    neofetch
    pandoc
    ripgrep
    rsync
    tmux
    zoxide
  ];

}
