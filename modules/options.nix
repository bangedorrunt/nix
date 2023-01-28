{ config, pkgs, inputs, lib, options, ... }:

with lib;

let
  inherit (lib.mine.options) mkOpt mkOpt' mkOptStr;

in
{
  options = with types; {
    my = {
      name = mkOptStr "Thanh Dung TRUONG";
      timezone = mkOptStr "AEST";
      username = mkOptStr "brunetdragon";
      website = mkOptStr "https://bangedorrunt.github.io";
      github_username = mkOptStr "bangedorrunt";
      email = mkOptStr "braden.truong@gmail.com";
      terminal = mkOptStr "WezTerm";
      user = mkOption { type = options.users.users.type.functor.wrapped; };
      hm = {
        file = mkOpt' attrs { } "Files to place directly in $HOME";
        configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
        dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
        packages = mkOpt (listOf package) [ ];
      };
    };
    hm = mkOption { type = options.home-manager.users.type.functor.wrapped; };
    # hm = mkOpt' attrs { } "Primary user's home-manager configuration";
  };

  config = {
    # my.user -> users.users.<primary user>
    users.users.${config.my.username} = mkAliasDefinitions options.my.user;
    my.user = {
      description = config.my.name;
      home =
        if pkgs.stdenv.isDarwin then
          "/Users/${config.my.username}"
        else
          "/home/${config.my.username}";

      shell = pkgs.fish;
    };

    # Let Nix manage Home-Manager profiles and use global nixpkgs
    home-manager = {
      # Don't set `useGlobalPkgs`! it's not neccessary in newer HM
      # and doens't work with configs using `nixpkgs.config`.
      # useGlobalPkgs = true;
      useUserPackages = true;
      # This caused undefined variable `hm` if I don't smash
      # home-manager.lib together with `nixpkgs.lib` as @kclejeune did
      extraSpecialArgs = { inherit inputs lib; };
      backupFileExtension = "backup";
    };
    # hm -> home-manager.users.<primary user>
    home-manager.users.${config.my.username} = mkAliasDefinitions options.hm;

    # I only need a subset of home-manager's capabilities. That is, access to
    # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
    # files easily to my $HOME, but 'home-manager.users.${config.my.username}.home.file.*'
    # is much too long and harder to maintain, so I've made aliases in:
    #
    #   my.hm.file        ->  home-manager.users.babygau.home.file
    #   my.hm.configFile  ->  home-manager.users.babygau.home.xdg.configFile
    #   my.hm.dataFile    ->  home-manager.users.babygau.home.xdg.dataFile
    #   my.hm.packages    ->  home-manager.users.babygau.home.packages
    hm = {
      # imports = [ ./shared ];
      xdg = {
        enable = true;
        configFile = mkAliasDefinitions options.my.hm.configFile;
        dataFile = mkAliasDefinitions options.my.hm.dataFile;
      };

      home = {
        # Necessary for home-manager to work with flakes, otherwise it will
        # look for a nixpkgs channel.
        stateVersion = if pkgs.stdenv.isDarwin then "23.05" else config.system.stateVersion;
        username = config.my.username;
        homeDirectory = config.my.user.home;
        file = mkAliasDefinitions options.my.hm.file;
        packages = mkAliasDefinitions options.my.hm.packages;

        sessionVariables = {
          # GPG_TTY = "/dev/ttys000";
          EDITOR = "nvim";
          VISUAL = "nvim";
          CLICOLOR = 1;
          LSCOLORS = "ExFxBxDxCxegedabagacad";
        };
      };

      programs = {
        # Let Home Manager install and manage itself.
        home-manager.enable = true;
        # home-manager.path = "${self}/modules/shared";
      };
    };
  };
}
