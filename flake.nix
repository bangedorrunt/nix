{
  description = "Nix Configurations";

  nixConfig = {
    substituters = [
      "https://cachix.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org/"
      "https://babygau.cachix.org"
    ];

    trusted-public-keys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "babygau.cachix.org-1:zkCsTYBSsJawIKOFe3pSYg1ZpOpwdln5DRLna7Ovx0A="
    ];
  };

  inputs = {
    # devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-stable.url = "github:nixos/nixpkgs/nixos-21.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin-stable.url = "github:nixos/nixpkgs/nixpkgs-21.05-darwin";
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
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    clojure-lsp.url = "github:zachcoyle/clojure-lsp-flake";
    treefmt.url = "github:numtide/treefmt";
    /* emacs-overlay = {
      type = "github";
      owner = "mjlbach";
      repo = "emacs-overlay";
      ref = "feature/flakes";
      inputs.nixpkgs.follows = "nixpkgs";
      }; */

    /* neuron = {
      type = "github";
      owner = "srid";
      repo = "neuron";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
      }; */
  };

  outputs =
    { self
    , nixpkgs
    , darwin
    , home-manager
      # , devshell
    , flake-utils
    , ...
    }@inputs:
    let
      # Extend nixpkgs.lib with custom lib and HM lib
      # Custom `./lib` will exposed as `lib.mine`
      #
      # NOTE: if you pass this lib in [darwin/home/nixos]Configurations which
      # using home-manager module, make sure you merge `home-manager.lib` 
      # otherwise build will fail. 
      # My guess is `lib` will override system lib, so some/all attributes of
      # system lib will be _undefined_, thus build error!
      mkLib = nixpkgs:
        nixpkgs.lib.extend
          (final: prev: { mine = import ./lib final; } // home-manager.lib);
      lib = (mkLib nixpkgs);

      inherit (darwin.lib) darwinSystem;
      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;

      # `listToAttrs` construct a set from a list specifying the names and values of each 
      # attribute. Each element of the list should be a set consisting of a string-valued
      # attribute name specifying the name of the attribute, and an attribute value 
      # specifying its value
      #
      # Example: given
      # builtins.listToAttrs [ { name = "foo"; value = 123; }
      #                        { name = "bar"; value = 456; } ]
      # ==> { foo = 123; bar = 456; }

      inherit (builtins) listToAttrs map;

      isDarwin = system: (builtins.elem system lib.platforms.darwin);
      homePrefix = system: if isDarwin system then "/Users" else "/home";
      supportedSystems = [ "x86_64-darwin" "x86_64-linux" ];

      # Generate a base darwin configuration with the
      # specified hostname, overlays, and any extraModules applied
      mkDarwinConfig =
        { system ? "x86_64-darwin"
        , nixpkgs ? inputs.nixpkgs
        , stable ? inputs.darwin-stable
        , lib ? (mkLib nixpkgs)
        , baseModules ? [
            # Use home-manager module
            home-manager.darwinModules.home-manager
            # NOTE: modules imported from here will inherit system context
            ./modules/common.nix
            ./modules/shared
          ]
        , extraModules ? [ ]
        }:
        darwinSystem {
          inherit system;
          modules = baseModules ++ extraModules;
          specialArgs = { inherit inputs lib nixpkgs stable; };
        };

      # Generate a home-manager configuration usable on any unix system
      # with overlays and any extraModules applied
      mkHomeConfig =
        { username
        , system ? "x86_64-linux"
        , nixpkgs ? inputs.nixpkgs
        , stable ? inputs.nixos-stable
        , lib ? (mkLib nixpkgs)
        , baseModules ? [ ./modules/hm ]
        , extraModules ? [ ]
        }:
        homeManagerConfiguration rec {
          inherit system username;
          homeDirectory = "${homePrefix system}/${username}";
          extraSpecialArgs = { inherit inputs lib nixpkgs stable; };
          configuration = {
            imports = baseModules ++ extraModules ++ [
              (
                import ./modules/overlays.nix {
                  inherit inputs nixpkgs stable;
                }
              )
            ];
          };
        };
    in
    {
      # lib = lib.mine;
      checks = listToAttrs (
        # Darwin checks
        (
          map
            (
              system: {
                name = system;
                value = {
                  darwin =
                    self.darwinConfigurations.me_at_home_with_macos.config.system.build.toplevel;
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
                  linux = self.homeConfigurations.me_at_home_with_linux.activationPackage;
                };
              }
            )
            lib.platforms.linux
        )
      );

      darwinConfigurations = {
        me_at_home_with_macos = mkDarwinConfig {
          extraModules = [ ./hosts/me_at_home_with_macos.nix ];
        };
      };
      homeConfigurations = {
        me_at_home_with_linux = mkHomeConfig {
          username = "babygau";
          extraModules = [ ./hosts/me_at_home_with_linux.nix ];
        };
      };
      me_at_home_with_macos =
        self.darwinConfigurations.me_at_home_with_macos.config.system.build.toplevel;
      me_at_home_with_linux =
        self.homeConfigurations.me_at_home_with_linux.activationPackage;
    };
}
