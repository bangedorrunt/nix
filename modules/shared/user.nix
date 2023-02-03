{
  config,
  pkgs,
  inputs,
  lib,
  options,
  ...
}:
with lib; {
  # tdt.user -> users.users.<primary user>
  users.users.${config.tdt.username} = mkAliasDefinitions options.tdt.user;
  tdt.user = {
    description = config.tdt.name;
    home =
      if pkgs.stdenv.isDarwin
      then "/Users/${config.tdt.username}"
      else "/home/${config.tdt.username}";

    shell = pkgs.fish;
  };
}
