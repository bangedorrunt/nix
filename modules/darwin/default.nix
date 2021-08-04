{ config, lib, pkgs, ... }: {
  imports = [
    ./core.nix
    ./brew.nix
    ./sensible_default_macos.nix
  ];
}
