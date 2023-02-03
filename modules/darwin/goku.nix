{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}: let
  cfg = config.tdt.modules.goku;
in {
  options = with lib; {
    tdt.modules.goku = {
      enable = mkEnableOption ''
        Whether to enable Goku module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      tdt.hm.packages = with pkgs; [goku];
    };
}
