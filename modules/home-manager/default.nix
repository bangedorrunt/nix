{ inputs, config, pkgs, ... }:
let
  homeDir = config.home.homeDirectory;
in
{
  imports = [ ./dotfiles.nix ];

  programs.home-manager = {
    enable = true;
    path = "${config.home.homeDirectory}/.nixpkgs/modules/home-manager";
  };

  home =
    {
      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      stateVersion = "20.09";
      sessionVariables = {
        GPG_TTY = "/dev/ttys000";
        EDITOR = "nvim";
        VISUAL = "nvim";
        CLICOLOR = 1;
        LSCOLORS = "ExFxBxDxCxegedabagacad";
      };

      # Define package definitions for current user environment
      packages = with pkgs; [
        # python with default packages
        (python39.withPackages (ps: with ps; [ black numpy scipy networkx ]))
        cachix
        coreutils-full
        curl
        curlie
        fd
        gawk
        ghc
        git
        gnugrep
        gnupg
        gnused
        htop
        httpie
        hyperfine
        jq
        neofetch
        niv
        nixUnstable
        nixfmt
        nixpkgs-fmt
        nodejs_latest
        openssh
        pandoc
        ripgrep
        ripgrep-all
        rsync
        yarn
      ];
    };
}
