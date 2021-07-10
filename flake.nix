{
  description = "Nix Configurations";

  nixConfig = {
    substituters = [
      "https://kclejeune.cachix.org"
      "https://nix-community.cachix.org/"
    ];

    trusted-public-keys = [
      "kclejeune.cachix.org-1:fOCrECygdFZKbMxHClhiTS6oowOkJ/I/dh9q9b1I4ko="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
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
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , stable
    , darwin
    , home-manager
    , devshell
    , flake-utils
    , ...
    }:
    let
      isDarwin = system: (builtins.elem system lib.platforms.darwin);
      homePrefix = system: if isDarwin system then "/Users" else "/home";

      supportedSystems = [ "x86_64-darwin" "x86_64-linux" ];
      overlays = [ inputs.neovim-nightly-overlay.overlay
                   inputs.emacs-overlay.overlay ];
      lib = nixpkgs.lib.extend
       (final: prev: home-manager.lib);

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
            ./modules/darwin
          ]
        , extraModules ? [ ]
        }:
        darwinSystem {
          # system = "x86_64-darwin";
          modules = baseModules ++ extraModules
            ++ [{ nixpkgs.overlays = overlays; }];
          specialArgs = { inherit inputs lib; };
        };

      # Generate a home-manager configuration usable on any unix system
      # with overlays and any extraModules applied
      mkHomeConfig =
        { username
        , system ? "x86_64-linux"
        , baseModules ? [ ./modules/home-manager ]
        , extraModules ? [ ]
        }:
        homeManagerConfiguration rec {
          inherit system username;
          homeDirectory = "/${homePrefix system}/${username}";
          extraSpecialArgs = { inherit inputs lib; };
          configuration = {
            imports = baseModules ++ extraModules
              ++ [{ nixpkgs.overlays = overlays; }];
          };
        };
    in
    {
      checks = listToAttrs (
        # Darwin checks
        (map
          (system: {
            name = system;
            value = {
              macos =
                self.darwinConfigurations.imac.config.system.build.toplevel;
            };
          })
          lib.platforms.darwin) ++
        # Linux checks
        (map
          (system: {
            name = system;
            value = {
              archlinux = self.homeConfigurations.archlinux.activationPackage;
            };
          })
          lib.platforms.linux)
      );

      darwinConfigurations = {
        imac = mkDarwinConfig {
          extraModules = [ ./profiles/personal.nix ./modules/darwin/apps.nix ];
        };
      };

      homeConfigurations = {
        archlinux = mkHomeConfig {
          username = "babygau";
          extraModules = [ ./profiles/home-manager/personal.nix ];
        };
      };
    };
}
