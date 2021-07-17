{ config, pkgs, inputs, lib, home-manager, options, ... }:

with lib;

let
  mkOptStr = value:
    mkOption {
      type = with types; uniq str;
      default = value;
    };

  mkSecret = description: default:
    mkOption {
      inherit description default;
      type = with types; either str (listOf str);
    };

  mkOpt = type: default: mkOption { inherit type default; };

  mkOpt' = type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt = default:
    mkOption {
      inherit default;
      type = types.bool;
      example = true;
    };

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
      nix_managed = mkOptStr
        "vim: set nomodifiable : Nix managed - DO NOT EDIT - see source inside ~/dotfiles or use `:set modifiable` to force.";
      user = mkOption { type = options.users.users.type.functor.wrapped; };
      hm = {
        file = mkOpt' attrs {} "Files to place directly in $HOME";
        configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
        dataFile = mkOpt' attrs {} "Files to place in $XDG_DATA_HOME";
        packages = mkOpt (listOf package) [];
      };
      env = mkOption {
        type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
        apply = mapAttrs (
          n: v:
            if isList v then
              concatMapStringsSep ":" (x: toString x) v
            else
              (toString v)
        );
        default = {};
        description = "TODO";
      };
    };
    hm = mkOpt attrs {};
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

      shell = pkgs.zsh;
    };
    # Let Nix manage Home-Manager profiles and use global nixpkgs
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
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
      # imports = [./shared];
      xdg = {
        enable = true;
        configFile = mkAliasDefinitions options.my.hm.configFile;
        dataFile = mkAliasDefinitions options.my.hm.dataFile;
      };

      home = {
        # Necessary for home-manager to work with flakes, otherwise it will
        # look for a nixpkgs channel.
        stateVersion =
          if pkgs.stdenv.isDarwin then "20.09" else config.system.stateVersion;
        username = config.my.username;
        file = mkAliasDefinitions options.my.hm.file;
        packages = mkAliasDefinitions options.my.hm.packages;

        sessionVariables = {
          GPG_TTY = "/dev/ttys000";
          EDITOR = "nvim";
          VISUAL = "nvim";
          CLICOLOR = 1;
          LSCOLORS = "ExFxBxDxCxegedabagacad";
        };
      };

      programs = {
        # Let Home Manager install and manage itself.
        home-manager.enable = true;
        home-manager.path = ./modules/shared;
      };
    };

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.my.env);
  };
}
