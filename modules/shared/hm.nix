{
  config,
  pkgs,
  inputs,
  lib,
  options,
  ...
}:
with lib; {
  # Let Nix manage Home-Manager profiles and use global nixpkgs
  home-manager = {
    # Don't set `useGlobalPkgs`! it's not neccessary in newer HM
    # and doens't work with configs using `nixpkgs.config`.
    # useGlobalPkgs = true;
    useUserPackages = true;
    # This caused undefined variable `hm` if I don't smash
    # home-manager.lib together with `nixpkgs.lib` as @kclejeune did
    extraSpecialArgs = {inherit inputs lib;};
    backupFileExtension = "backup";
  };
  # hm -> home-manager.users.<primary user>
  home-manager.users.${config.tdt.username} = mkAliasDefinitions options.hm;

  # I only need a subset of home-manager's capabilities. That is, access to
  # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
  # files easily to my $HOME, but 'home-manager.users.${config.tdt.username}.home.file.*'
  # is much too long and harder to maintain, so I've made aliases in:
  #
  #   tdt.hm.file        ->  home-manager.users.brunetdragon.home.file
  #   tdt.hm.configFile  ->  home-manager.users.brunetdragon.home.xdg.configFile
  #   tdt.hm.dataFile    ->  home-manager.users.brunetdragon.home.xdg.dataFile
  #   tdt.hm.packages    ->  home-manager.users.brunetdragon.home.packages
  hm = {
    # imports = [ ./shared ];
    xdg = {
      enable = true;
      configFile = mkAliasDefinitions options.tdt.hm.configFile;
      dataFile = mkAliasDefinitions options.tdt.hm.dataFile;
    };

    home = {
      # Necessary for home-manager to work with flakes, otherwise it will
      # look for a nixpkgs channel.
      stateVersion =
        if pkgs.stdenv.isDarwin
        then "23.05"
        else config.system.stateVersion;
      username = config.tdt.username;
      homeDirectory = config.tdt.user.home;
      file = mkAliasDefinitions options.tdt.hm.file;
      packages = mkAliasDefinitions options.tdt.hm.packages;

      sessionVariables = {
        # GPG_TTY = "/dev/ttys000";
        EDITOR = "nvim";
        VISUAL = "nvim";
        CLICOLOR = 1;
        LSCOLORS = "ExFxBxDxCxegedabagacad";
      };
    };

    # NOTE there's never any reason to set programs.home-manager.enable when
    # using as a nixOS|darwin module, except maybe to bootstrap switching to
    # standalone
    # programs.home-manager.enable = true;
    # programs.home-manager.path = "${self}/modules/shared";
  };
}
