# TODO split this into `modules.hm` and `modules.user`
{
  config,
  pkgs,
  inputs,
  _inputs,
  lib,
  options,
  ...
}:
with lib; let
  inherit (lib.mine) mkOpt mkOpt' mkOptStr;
in {
  options = with types; {
    tdt = {
      name = mkOptStr "Thanh Dung TRUONG";
      timezone = mkOptStr "AEST";
      username = mkOptStr "brunetdragon";
      website = mkOptStr "https://bangedorrunt.github.io";
      github_username = mkOptStr "bangedorrunt";
      email = mkOptStr "9uermpgz@duck.com";
      terminal = mkOptStr "WezTerm";
      user = mkOption {type = options.users.users.type.functor.wrapped;};
      hm = {
        file = mkOpt' attrs {} "Files to place directly in $HOME";
        configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
        dataFile = mkOpt' attrs {} "Files to place in $XDG_DATA_HOME";
        packages = mkOpt (listOf package) [];
      };
    };
    hm = mkOption {type = options.home-manager.users.type.functor.wrapped;};
    # hm = mkOpt' attrs { } "Primary user's home-manager configuration";
  };
}
