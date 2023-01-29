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
    # Package repo
    stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";

    # System management
    # TODO using `flake.parts`
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nixos-generators = {
      #url = "github:nix-community/nixos-generators";
      url = "github:Mic92/nixos-generators/fedf7136f27490402fe8ab93e67fafae80513e9b";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    # Shell stuff
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix.url = "github:numtide/treefmt-nix";
    # Editors
    emacs.url = "github:cmacrae/emacs";
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.url =
        "github:nixos/nixpkgs?rev=fad51abd42ca17a60fc1d4cb9382e2d79ae31836";
    };
  };
  outputs =
    { self
    , nixpkgs
    , darwin
    , home-manager
    , flake-parts
    , flake-utils
    , treefmt-nix
    , disko
    , ...
    } @ inputs:
    let
      # Extend nixpkgs.lib with custom lib and HM lib
      # Custom `./lib` will exposed as `lib.mine`
      #
      # NOTE if you pass this lib in [darwin/home/nixos]Configurations which
      # using home-manager module, make sure you merge `home-manager.lib`
      # otherwise build will fail.
      # My guess is `lib` will override system lib, so some/all attributes of
      # system lib will be _undefined_, thus build error!
      mkLib = nixpkgs:
        nixpkgs.lib.extend
          (final: prev: { mine = import ./lib final; } // home-manager.lib);
      lib = (mkLib inputs.nixpkgs);

      inherit (flake-utils.lib) eachSystemMap;
      inherit (lib.mine) rakeLeaves;

      hosts = rakeLeaves ./hosts;
      modules = rakeLeaves ./modules;

      defaultSystems =
        [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      # REFACTOR move to ./lib/host.nix
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
            modules.common
            modules.shared
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
            disko.nixosModules.disko
            modules.common
            modules.shared
          ]
        , extraModules ? [ ]
        ,
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          modules = baseModules ++ hardwareModules ++ extraModules;
          specialArgs = { inherit self inputs lib nixpkgs; };
        };
    in
    {
      darwinConfigurations = {
        # NOTE reserve for future device
        # "brunetdragon@aarch64-darwin" = mkDarwinConfig {
        #   system = "aarch64-darwin";
        #   extraModules = [ ];
        # };
        "brunetdragon@x86_64-darwin" = mkDarwinConfig {
          system = "x86_64-darwin";
          extraModules = [ hosts."brunetdragon@x86_64-darwin" ];
        };
      };

      # otherwise `cirrus-ci` will raise error
      nixosConfigurations = {
        # NOTE reserve for future device
        # "brunetdragon@aarch64-linux" = mkNixosConfig {
        #   system = "x86_64-linux";
        #   hardwareModules = [ modules.nixos ];
        #   extraModules = [ ];
        # };
        "brunetdragon@x86_64-linux" = mkNixosConfig {
          system = "x86_64-linux";
          hardwareModules = [ modules.nixos ];
          extraModules = [ hosts."brunetdragon@x86_64-linux" ];
        };
      };

      # WARNING unknown flake output but for convenience
      # build steps:
      # nix --extra-experimental-features 'nix-command flakes' build .\#brunetdragon@x86_64-darwin
      # ./result/sw/bin/darwin-rebuild switch --flake .\#brunetdragon@x86_64-darwin
      "brunetdragon@x86_64-darwin" =
        self.darwinConfigurations."brunetdragon@x86_64-darwin".config.system.build.toplevel;

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
              pkgs.nil
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
