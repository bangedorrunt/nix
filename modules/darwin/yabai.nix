{ config, pkgs, ... }:

{
  services.yabai.enable = true;
  services.yabai.package = pkgs.yabai;
}
