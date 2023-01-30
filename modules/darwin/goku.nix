{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}: let
  cfg = config.my.modules.goku;
in {
  options = with lib; {
    my.modules.goku = {
      enable = mkEnableOption ''
        Whether to enable Goku module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.hm.packages = with pkgs; [goku];
    };
}
