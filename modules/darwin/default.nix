{ config, lib, pkgs, ... }: {
  imports = [
    ./core.nix
    ./brew.nix
    ./apps.nix
    ./sensible-defaults-macos.nix
  ];
}
