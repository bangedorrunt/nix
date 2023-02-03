{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.tdt.modules.yabai;
in {
  options = with lib; {
    tdt.modules.yabai = {
      enable = mkEnableOption ''
        Whether to enable Yabai module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      services.yabai.package = pkgs.yabai;
      services.yabai.enable = true;
    };
}
