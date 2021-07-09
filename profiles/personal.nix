{ config, lib, pkgs, ... }: {
  user.name = "babygau";
  hm = { imports = [ ./home-manager/personal.nix ]; };
}
