{ config, lib, pkgs, ... }:

let
  cfg = config.my.modules.macos;
in

{
  options = with lib; {
    my.modules.macos = {
      enable = mkEnableOption ''
        Whether to enable Yabai module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      # Install Yabai
      services.yabai.package = pkgs.yabai;
      services.yabai.enable = true;
      # Install Goku
      my.hm.packages = with pkgs; [ goku ];
    };
}
