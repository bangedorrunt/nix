{ config, inputs, lib, pkgs, options, ... }:
{
  imports = [
    ./dotfiles.nix
    ./dev.nix
  ];

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
    kepubify
    neofetch
    # nixUnstable
    # openssh
    pandoc
    ripgrep
    rsync
    tmux
    zoxide
  ];

}
