{
  self,
  inputs,
  _inputs,
  lib,
  withSystem,
  ...
}: let
  inherit (inputs) home-manager disko;
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
      specialArgs = {inherit self inputs lib;};
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
    "brunetdragon@x86_64-darwin" =
      withSystem "x86_64-darwin"
      ({
        pkgs,
        system,
        ...
      }:
        mkDarwinConfig {
          extraModules = [hosts."brunetdragon@x86_64-darwin"];
        });
  };
  flake.nixosConfigurations = {
    "brunetdragon@x86_64-linux" =
      withSystem "x86_64-linux"
      ({
        pkgs,
        system,
        ...
      }:
        mkNixosConfig {
          hardwareModules = [modules.nixos];
          extraModules = [hosts."brunetdragon@x86_64-linux"];
        });
  };

  # WARNING unknown flake output but for convenience
  # build steps:
  # nix --extra-experimental-features 'nix-command flakes' build .\#brunetdragon@x86_64-darwin
  # ./result/sw/bin/darwin-rebuild switch --flake .\#brunetdragon@x86_64-darwin
  flake."brunetdragon@x86_64-darwin" =
    self.darwinConfigurations."brunetdragon@x86_64-darwin".config.system.build.toplevel;
}
