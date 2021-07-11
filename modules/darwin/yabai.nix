{ config, pkgs, ... }:

{
  services.yabai.enable = true;
  services.yabai.package = pkgs.yabai;
  services.skhd.enable = false;

  services.spacebar.enable = false;
  services.spacebar.package = pkgs.spacebar;
}
