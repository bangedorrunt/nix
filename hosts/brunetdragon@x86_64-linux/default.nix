{
  inputs,
  nixpkgs,
  pkgs,
  config,
  lib,
  ...
}: {
  # TODO generate minimal nixOS configuration
  imports = [
    ./boot.nix
    ./disko.nix
    ./hardware.nix
    ./users.nix
    ./system.nix
  ];
}
