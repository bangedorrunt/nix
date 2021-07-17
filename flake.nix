{
  description = "Nix Configurations";

  nixConfig = {
    substituters = [
      "https://cachix.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org/"
      "https://srid.cachix.org"
      "https://kclejeune.cachix.org"
    ];

    trusted-public-keys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "srid.cachix.org-1:MTQ6ksbfz3LBMmjyPh0PLmos+1x+CdtJxA/J2W+PQxI="
      "kclejeune.cachix.org-1:fOCrECygdFZKbMxHClhiTS6oowOkJ/I/dh9q9b1I4ko="
    ];
  };

  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    stable.url = "github:nixos/nixpkgs/nixos-20.09";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      type = "github";
      owner = "mjlbach";
      repo = "emacs-overlay";
      ref = "feature/flakes";
      # inputs.nixpkgs.follows = "nixpkgs";
    };
    neuron = {
      type = "github";
      owner = "srid";
      repo = "neuron";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , stable
    , darwin
    , home-manager
    , devshell
    , flake-utils
    , neuron
    , ...
    }:
      let
        isDarwin = system: (builtins.elem system lib.platforms.darwin);
        homePrefix = system: if isDarwin system then "/Users" else "/home";

        supportedSystems = [ "x86_64-darwin" "x86_64-linux" ];
        overlays = [
          inputs.neovim-nightly-overlay.overlay
          inputs.emacs-overlay.overlay
          (import ./modules/pkgs/yabai_overlay.nix)
          (
            final: prev: {
              neuron = (prev.callPackage "${inputs.neuron}/project.nix" {}).neuron;
            }
          )
          (import ./modules/pkgs/sumneko_overlay.nix)
        ];
        lib = nixpkgs.lib.extend
          (final: prev: {} // home-manager.lib);

        inherit (darwin.lib) darwinSystem;
        inherit (nixpkgs.lib) nixosSystem;
        inherit (home-manager.lib) homeManagerConfiguration;
        inherit (flake-utils.lib) eachDefaultSystem eachSystem;
        inherit (builtins) listToAttrs map;

        # Generate a base darwin configuration with the
        # specified hostname, overlays, and any extraModules applied
        mkDarwinConfig =
          { system ? "x86_64-darwin"
          , baseModules ? [
              home-manager.darwinModules.home-manager
              ./modules/common.nix
              ./modules/shared
              ./modules/darwin
            ]
          , extraModules ? []
          }:
            darwinSystem {
              # system = "x86_64-darwin";
              modules = baseModules ++ extraModules
              ++ [ { nixpkgs.overlays = overlays; } ];
              specialArgs = { inherit inputs lib; };
            };

        # Generate a home-manager configuration usable on any unix system
        # with overlays and any extraModules applied
        mkHomeConfig =
          { username
          , system ? "x86_64-linux"
          , baseModules ? [ ./modules/common.nix ./modules/shared ]
          , extraModules ? []
          }:
            homeManagerConfiguration rec {
              inherit system username;
              homeDirectory = "/${homePrefix system}/${username}";
              # extraSpecialArgs = { inherit inputs lib; };
              configuration = {
                imports = baseModules ++ extraModules
                ++ [ { nixpkgs.overlays = overlays; } ];
              };
            };
      in
        {
          checks = listToAttrs (
            # Darwin checks
            (
              map
                (
                  system: {
                    name = system;
                    value = {
                      darwin =
                        self.darwinConfigurations.imac.config.system.build.toplevel;
                    };
                  }
                )
                lib.platforms.darwin
            ) ++ # Linux checks
            (
              map
                (
                  system: {
                    name = system;
                    value = {
                      archlinux = self.homeConfigurations.archlinux.activationPackage;
                    };
                  }
                )
                lib.platforms.linux
            )
          );

          darwinConfigurations = {
            imac = mkDarwinConfig {
              extraModules = [ ./modules/darwin/apps.nix ];
            };
          };

          homeConfigurations = {
            archlinux = mkHomeConfig {
              username = "babygau";
            };
          };
        };
}
