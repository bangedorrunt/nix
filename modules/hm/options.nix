# NOTE: please compare `modules/hm/options.nix` and `modules/options.nix` to see
# the difference between home-manager as [nixos/darwin] module and home-manager
# itself
{ config, system, pkgs, inputs, lib, options, ... }:

with lib;

let
  inherit (lib.mine.options) mkOpt mkOpt' mkOptStr mkBoolOpt;
in
{
  options = with types; {
    my = {
      name = mkOptStr "Thanh Dung TRUONG";
      timezone = mkOptStr "AEST";
      username = mkOptStr "babygau";
      website = mkOptStr "https://babygau.github.io";
      github_username = mkOptStr "babygau";
      email = mkOptStr "braden.truong@gmail.com";
      terminal = mkOptStr "iTerm2";
      # home-manager doesn't suppport this options
      # user = mkOption { type = options.users.users.type.functor.wrapped; };
      user = mkOpt' attrs { } "Primary user";
      hm = {
        file = mkOpt' attrs { } "Files to place directly in $HOME";
        configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
        dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
        packages = mkOpt (listOf package) [ ];
      };
    };
  };

  config = {
    my.user = {
      description = config.my.name;
      home =
        if pkgs.stdenv.isDarwin then
          "/Users/${config.my.username}"
        else
          "/home/${config.my.username}";

      shell = pkgs.zsh;
    };

    # NOTE: home-manager is a module which is used in
    # [darwin/nixos]Configurations, it is not valid in homeConfigurations
    #
    # hm -> home-manager.users.<primary user>
    # home-manager.users.${config.my.username} = mkAliasDefinitions options.hm;

    # I only need a subset of home-manager's capabilities. That is, access to
    # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
    # files easily to my $HOME, but 'home-manager.users.${config.my.username}.home.file.*'
    # is much too long and harder to maintain, so I've made aliases in:
    #
    #   my.hm.file        ->  home-manager.users.babygau.home.file
    #   my.hm.configFile  ->  home-manager.users.babygau.home.xdg.configFile
    #   my.hm.dataFile    ->  home-manager.users.babygau.home.xdg.dataFile
    #   my.hm.packages    ->  home-manager.users.babygau.home.packages
    xdg = {
      enable = true;
      configFile = mkAliasDefinitions options.my.hm.configFile;
      dataFile = mkAliasDefinitions options.my.hm.dataFile;
    };

    home = {
      # Necessary for home-manager to work with flakes, otherwise it will
      # look for a nixpkgs channel.
      # stateVersion = "21.05";
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
      # home-manager.path = https://github.com/rycee/home-manager/master.tar.gz;
    };
  };
}
