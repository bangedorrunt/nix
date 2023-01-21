# TODO up to date nix after a long time
{
  description = "Nix Configurations";

  nixConfig = {
    substituters = [
      "https://cachix.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org/"
    ];
    trusted-public-keys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };

  inputs = {
    # package repo
    stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";

    # system management
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # shell stuff
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix.url = "github:numtide/treefmt-nix";

    # overlays
    # neovim-nightly-overlay = {
    #   url = "github:nix-community/neovim-nightly-overlay";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    # emacs-overlay = {
    #   type = "github";
    #   owner = "mjlbach";
    #   repo = "emacs-overlay";
    #   ref = "feature/flakes";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    clojure-lsp.url = "github:zachcoyle/clojure-lsp-flake";
  };

  outputs = inputs @ {
    self,
    darwin,
    home-manager,
    flake-utils,
    treefmt-nix,
    ...
    }:
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
      lib = (mkLib inputs.nixpkgs);

      inherit (flake-utils.lib) eachSystemMap;

      isDarwin = system: (builtins.elem system inputs.nixpkgs.lib.platforms.darwin);
      homePrefix = system:
        if isDarwin system
        then "/Users"
        else "/home";
      defaultSystems = [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      # Generate a base darwin configuration with the
      # specified hostname, overlays, and any extraModules applied
      mkDarwinConfig =
        { system ? "x86_64-darwin"
        , nixpkgs ? inputs.nixpkgs
        , stable ? inputs.stable
        , lib ? (mkLib inputs.nixpkgs)
        , baseModules ? [
            # Use home-manager module
            home-manager.darwinModules.home-manager
            # NOTE modules imported from here will inherit system context
            ./modules/common.nix
            ./modules/shared
          ]
        , extraModules ? [ ]
        }:
        inputs.darwin.lib.darwinSystem {
          inherit system;
          modules = baseModules ++ extraModules;
          specialArgs = { inherit self inputs lib nixpkgs; };
        };

      # generate a base nixos configuration with the
      # specified overlays, hardware modules, and any extraModules applied
      mkNixosConfig =
        { system ? "x86_64-linux"
        , nixpkgs ? inputs.nixos-unstable
        , stable ? inputs.stable
        , lib ? (mkLib inputs.nixpkgs)
        , hardwareModules
        , baseModules ? [
            home-manager.nixosModules.home-manager
            ./modules/common.nix
            ./modules/shared
          ]
        , extraModules ? [ ]
        ,
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          modules = baseModules ++ hardwareModules ++ extraModules;
          specialArgs = { inherit self inputs lib nixpkgs; };
        };

      # Generate a home-manager configuration usable on any unix system
      # with overlays and any extraModules applied
      mkHomeConfig =
        { username
        , system ? "x86_64-linux"
        , nixpkgs ? inputs.nixpkgs
        , stable ? inputs.stable
        , lib ? (mkLib inputs.nixpkgs)
        , baseModules ? [
            ./modules/hm
            {
              home = {
                inherit username;
                homeDirectory = "${homePrefix system}/${username}";
                sessionVariables = {
                  NIX_PATH = "nixpkgs=${nixpkgs}:stable=${stable}\${NIX_PATH:+:}$NIX_PATH";
                };
              };
            }
          ]
        , extraModules ? [ ]
        ,
        }:
        inputs.home-manager.lib.homeManagerConfiguration rec {
          pkgs = import nixpkgs {
            inherit system;
            overlays = builtins.attrValues self.overlays;
          };
          extraSpecialArgs = { inherit self inputs lib nixpkgs; };
          modules = baseModules ++ extraModules;
        };
      mkChecks =
        { arch
        , os
        , username ? "brunetdragon"
        ,
        }: {
          "${arch}-${os}" = {
            "${username}_${os}" =
              (
                if os == "darwin"
                then self.darwinConfigurations
                else self.nixosConfigurations
              )."${username}@${arch}-${os}".config.system.build.toplevel;
            "${username}_home" =
              self.homeConfigurations."${username}@${arch}-${os}".activationPackage;
            devShell = self.devShells."${arch}-${os}".default;
          };
        };
    in
    {
      # lib = lib.mine;
      checks =
        { }
        // (mkChecks {
          arch = "aarch64";
          os = "darwin";
        })
        // (mkChecks {
          arch = "x86_64";
          os = "darwin";
        })
        // (mkChecks {
          arch = "aarch64";
          os = "linux";
        })
        // (mkChecks {
          arch = "x86_64";
          os = "linux";
        });

      darwinConfigurations = {
        "brunetdragon@aarch64-darwin" = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [ ./hosts/macos.nix ];
        };
        "brunetdragon@x86_64-darwin" = mkDarwinConfig {
          system = "x86_64-darwin";
          extraModules = [ ./hosts/macos.nix ];
        };
      };

      nixosConfigurations = {
        "brunetdragon@x86_64-linux" = mkNixosConfig {
          system = "x86_64-linux";
          hardwareModules = [
            ./modules/hardware/phil.nix
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t460s
          ];
          extraModules = [ ./hosts/linux.nix ];
        };
        "brunetdragon@aarch64-linux" = mkNixosConfig {
          system = "aarch64-linux";
          hardwareModules = [ ./modules/hardware/phil.nix ];
          extraModules = [ ./hosts/linux.nix ];
        };
      };

      homeConfigurations = {
        "brunetdragon@x86_64-linux" = mkHomeConfig {
          username = "brunetdragon";
          system = "x86_64-linux";
          extraModules = [ ./hosts/linux.nix ];
        };
        "brunetdragon@aarch64-linux" = mkHomeConfig {
          username = "brunetdragon";
          system = "aarch64-linux";
          extraModules = [ ./hosts/linux.nix ];
        };
        "brunetdragon@x86_64-darwin" = mkHomeConfig {
          username = "brunetdragon";
          system = "x86_64-darwin";
          extraModules = [ ./hosts/macos.nix ];
        };
        "brunetdragon@aarch64-darwin" = mkHomeConfig {
          username = "brunetdragon";
          system = "aarch64-darwin";
          extraModules = [ ./hosts/macos.nix ];
        };
      };
      "brunetdragon@x86_64-darwin" = self.darwinConfigurations."brunetdragon@x86_64-darwin".config.system.build.toplevel;
      "brunetdragon@x86_64-linux" = self.homeConfigurations."brunetdragon@x86_64-linux".activationPackage;
      overlays = {
        channels = final: prev: {
          # expose other channels via overlays
          stable = import inputs.stable { system = prev.system; };
          small = import inputs.small { system = prev.system; };
        };
        devshell = inputs.devshell.overlay;
        # inputs.neovim-nightly-overlay.overlay
      };
    };
}
