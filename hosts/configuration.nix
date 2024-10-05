{
  self,
  inputs,
  _inputs,
  lib,
  withSystem,
  ...
}: let
  inherit (inputs) home-manager disko nixos-raspberrypi;
  inherit (lib.mine) rakeLeaves;
  hosts = rakeLeaves ../hosts;
  modules = rakeLeaves ../modules;

  # Generate a base darwin configuration with the
  # specified hostname, overlays, and any extraModules applied
  mkDarwinConfig = {
    system ? "x86_64-darwin",
    nixpkgs ? inputs.nixpkgs,
    stable ? inputs.stable,
    baseModules ? [
      # Use home-manager module
      home-manager.darwinModules.home-manager
      modules.shared
    ],
    extraModules ? [],
  }:
    inputs.darwin.lib.darwinSystem {
      inherit system;
      modules = baseModules ++ extraModules;
      specialArgs = {inherit self inputs lib nixpkgs;};
    };

  # Generate a base nixos configuration with the
  # specified overlays, hardware modules, and any extraModules applied
  mkNixosConfig = {
    system ? "x86_64-linux",
    nixpkgs ? inputs.nixos-unstable,
    stable ? inputs.stable,
    hardwareModules,
    baseModules ? [
      home-manager.nixosModules.home-manager
      disko.nixosModules.disko
      modules.shared
    ],
    extraModules ? [],
  }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      modules = baseModules ++ hardwareModules ++ extraModules;
      specialArgs = {inherit self inputs lib;};
    };
in {
  flake.darwinConfigurations = {
    "brunetdragon@macintel" =
      withSystem "x86_64-darwin"
      ({...}:
        mkDarwinConfig {
          extraModules = [hosts."brunetdragon@macos"];
        });
  };
  flake.nixosConfigurations = {
    "brunetdragon@pifi" =
      withSystem "aarch64-linux"
      ({...}:
        mkNixosConfig {
          hardwareModules = [
            nixos-raspberrypi.nixosModules.raspberry-pi-5.base
            nixos-raspberrypi.lib.inject-overlays
            modules.nixos
          ];
          extraModules = [hosts."brunetdragon@pifi"];
        });
  };

  # WARNING unknown flake output but for convenience
  # build steps:
  # nix --extra-experimental-features 'nix-command flakes' build .\#brunetdragon@x86_64-darwin
  # ./result/sw/bin/darwin-rebuild switch --flake .\#brunetdragon@x86_64-darwin
  flake."brunetdragon@macintel" =
    self.darwinConfigurations."brunetdragon@macintel".config.system.build.toplevel;
  flake."brunetdragon@pifi" =
    self.nixosConfigurations."brunetdragon@pifi".config.system.build.toplevel;
}
