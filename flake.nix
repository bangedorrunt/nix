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
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
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
      url = "github:LnL7/nix-darwin";
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

    # editor
    emacs.url = "github:cmacrae/emacs";
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.url =
        "github:nixos/nixpkgs?rev=fad51abd42ca17a60fc1d4cb9382e2d79ae31836";
    };
  };

  outputs =
    inputs@{ self, darwin, home-manager, flake-utils, treefmt-nix, ... }:
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

      isDarwin = system:
        (builtins.elem system inputs.nixpkgs.lib.platforms.darwin);
      homePrefix = system: if isDarwin system then "/Users" else "/home";
      defaultSystems =
        [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      # Generate a base darwin configuration with the
      # specified hostname, overlays, and any extraModules applied
      mkDarwinConfig =
        { system ? "x86_64-darwin"
        , nixpkgs ? inputs.nixpkgs
        , stable ? inputs.stable
          # , lib ? (mkLib inputs.nixpkgs)
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
          # , lib ? (mkLib inputs.nixpkgs)
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
          # , lib ? (mkLib inputs.nixpkgs)
        , baseModules ? [
            ./modules/hm
            {
              home = {
                inherit username;
                homeDirectory = "${homePrefix system}/${username}";
                sessionVariables = {
                  NIX_PATH =
                    "nixpkgs=${nixpkgs}:stable=${stable}\${NIX_PATH:+:}$NIX_PATH";
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
            "${username}_${os}" = (if os == "darwin" then
              self.darwinConfigurations
            else
              self.nixosConfigurations)."${username}@${arch}-${os}".config.system.build.toplevel;
            "${username}_home" =
              self.homeConfigurations."${username}@${arch}-${os}".activationPackage;
            devShell = self.devShells."${arch}-${os}".default;
          };
        };
    in
    {
      # lib = lib.mine;
      checks = { } // (mkChecks {
        arch = "aarch64";
        os = "darwin";
      }) // (mkChecks {
        arch = "x86_64";
        os = "darwin";
      }) // (mkChecks {
        arch = "aarch64";
        os = "linux";
      }) // (mkChecks {
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

      # TODO generate nixOS configuration
      # otherwise `cirrus-ci` will raise error
      # nixosConfigurations = {
      #   "brunetdragon@x86_64-linux" = mkNixosConfig {
      #     system = "x86_64-linux";
      #     hardwareModules =
      #       [ inputs.nixos-hardware.nixosModules.raspberry-pi-4 ];
      #     extraModules = [ ./hosts/linux.nix ];
      #   };
      # };

      homeConfigurations = {
        "brunetdragon@x86_64-linux-hm" = mkHomeConfig {
          username = "brunetdragon";
          system = "x86_64-linux";
          extraModules = [ ];
        };
        "brunetdragon@aarch64-linux-hm" = mkHomeConfig {
          username = "brunetdragon";
          system = "aarch64-linux";
          extraModules = [ ];
        };
        "brunetdragon@x86_64-darwin-hm" = mkHomeConfig {
          username = "brunetdragon";
          system = "x86_64-darwin";
          extraModules = [ ];
        };
        "brunetdragon@aarch64-darwin-hm" = mkHomeConfig {
          username = "brunetdragon";
          system = "aarch64-darwin";
          extraModules = [ ];
        };
      };
      # build steps:
      # nix --extra-experimental-features 'nix-command flakes' build .\#brunetdragon@x86_64-darwin
      # ./result/sw/bin/darwin-rebuild switch --flake .\#brunetdragon@x86_64-darwin
      "brunetdragon@x86_64-darwin" =
        self.darwinConfigurations."brunetdragon@x86_64-darwin".config.system.build.toplevel;
      "brunetdragon@x86_64-darwin-hm" =
        self.homeConfigurations."brunetdragon@x86_64-darwin-hm".activationPackage;

      # TODO learn more about devshell
      devShells = eachSystemMap defaultSystems (system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = builtins.attrValues self.overlays;
          };
        in
        {
          default = pkgs.devshell.mkShell {
            packages = [
              pkgs.nixfmt
              pkgs.pre-commit
              pkgs.rnix-lsp
              self.packages.${system}.pyEnv
              (treefmt-nix.lib.mkWrapper pkgs (import ./treefmt.nix))
            ];
            commands = [
              {
                name = "sysdo";
                package = self.packages.${system}.sysdo;
                category = "utilities";
                help = "perform actions on this repository";
              }
            ];
          };
        });

      packages = eachSystemMap defaultSystems (system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = builtins.attrValues self.overlays;
          };
        in
        rec {
          pyEnv =
            pkgs.python3.withPackages
              (ps: with ps; [ black typer colorama shellingham ]);
          sysdo = pkgs.writeScriptBin "sysdo" ''
            #! ${pyEnv}/bin/python3
            ${builtins.readFile ./bin/do.py}
          '';
          cb = pkgs.writeShellScriptBin "cb" ''
            #! ${pkgs.lib.getExe pkgs.bash}
            # universal clipboard, stephen@niedzielski.com

            shopt -s expand_aliases

            # ------------------------------------------------------------------------------
            # os utils

            case "$OSTYPE$(uname)" in
              [lL]inux*) TUX_OS=1 ;;
             [dD]arwin*) MAC_OS=1 ;;
              [cC]ygwin) WIN_OS=1 ;;
                      *) echo "unknown os=\"$OSTYPE$(uname)\"" >&2 ;;
            esac

            is_tux() { [ ''${TUX_OS-0} -ne 0 ]; }
            is_mac() { [ ''${MAC_OS-0} -ne 0 ]; }
            is_win() { [ ''${WIN_OS-0} -ne 0 ]; }

            # ------------------------------------------------------------------------------
            # copy and paste

            if is_mac; then
              alias cbcopy=pbcopy
              alias cbpaste=pbpaste
            elif is_win; then
              alias cbcopy=putclip
              alias cbpaste=getclip
            else
              alias cbcopy='${pkgs.xclip} -sel c'
              alias cbpaste='${pkgs.xclip} -sel c -o'
            fi

            # ------------------------------------------------------------------------------
            cb() {
              if [ ! -t 0 ] && [ $# -eq 0 ]; then
                # no stdin and no call for --help, blow away the current clipboard and copy
                cbcopy
              else
                cbpaste ''${@:+"$@"}
              fi
            }

            # ------------------------------------------------------------------------------
            if ! return 2>/dev/null; then
              cb ''${@:+"$@"}
            fi
          '';
        });

      apps = eachSystemMap defaultSystems (system: rec {
        sysdo = {
          type = "app";
          program = "${self.packages.${system}.sysdo}/bin/sysdo";
        };
        cb = {
          type = "app";
          program = "${self.packages.${system}.cb}/bin/cb";
        };
        default = sysdo;
      });

      overlays = {
        channels = final: prev: {
          # expose other channels via overlays
          stable = import inputs.stable { system = prev.system; };
          small = import inputs.small { system = prev.system; };
        };

        extraPackages = final: prev: {
          sysdo = self.packages.${prev.system}.sysdo;
          pyEnv = self.packages.${prev.system}.pyEnv;
          cb = self.packages.${prev.system}.cb;
        };
        devshell = inputs.devshell.overlay;
        emacs = inputs.emacs.overlay;
        neovim = inputs.neovim-nightly-overlay.overlay;
      };
    };
}
