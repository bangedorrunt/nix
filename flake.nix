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
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # System management
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    flake-root.url = "github:srid/flake-root";
    flake-registry = {
      url = "github:NixOS/flake-registry";
      flake = false;
    };
    nixos-generators = {
      #url = "github:nix-community/nixos-generators";
      url = "github:Mic92/nixos-generators/fedf7136f27490402fe8ab93e67fafae80513e9b";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    impermanence = {
      url = "github:nix-community/impermanence";
    };
    disko = {
      url = "github:nix-community/disko";
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
    mission-control.url = "github:Platonic-Systems/mission-control";
    # Editors
    # emacs.url = "github:cmacrae/emacs";
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.url = "github:nixos/nixpkgs?rev=fad51abd42ca17a60fc1d4cb9382e2d79ae31836";
    };
  };

  outputs = {
    self,
    nixpkgs,
    darwin,
    home-manager,
    flake-parts,
    flake-root,
    flake-utils,
    treefmt-nix,
    mission-control,
    disko,
    ...
  } @ inputs: let
    # Extend nixpkgs.lib with custom lib and HM lib
    # Custom `./lib` will exposed as `lib.mine`
    # NOTE merge with `home-manager.lib` otherwise build will fail.
    # My guess is `lib` will override system lib, so some/all attributes of
    # system lib will be _undefined_, thus build error!
    mkLib = nixpkgs:
      nixpkgs.lib.extend
      (self: super: {mine = import ./lib {lib = self;};} // home-manager.lib);
    lib = mkLib inputs.nixpkgs;
    inherit (lib.mine) rakeLeaves;

    hosts = rakeLeaves ./hosts;
    modules = rakeLeaves ./modules;
  in
    flake-parts.lib.mkFlake
    {
      inherit inputs;
      specialArgs = {inherit lib;};
    }
    {
      # Imports flake-parts modules
      imports = [
        treefmt-nix.flakeModule
        flake-root.flakeModule
        mission-control.flakeModule
        flake-parts.flakeModules.easyOverlay
        hosts.configuration
        modules.devshells
        modules.formatter
        modules.scripts
      ];

      systems = [
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      # flake top-level configuration
      flake = {
        # NOTE overlays are overused, you might not need it
        overlays = {
          channels = final: prev: {
            # expose other channels via overlays
            stable = import inputs.stable {system = prev.system;};
            small = import inputs.small {system = prev.system;};
          };
          devshell = inputs.devshell.overlay;
          # emacs = inputs.emacs.overlay;
          neovim = inputs.neovim-nightly-overlay.overlay;
        };
      };

      # REVIEW flake-part steal `self`, drop when this is fixed
      _module.args._inputs = inputs // {inherit self;};

      perSystem = {
        inputs',
        pkgs,
        ...
      }: {
        # make pkgs available to all `perSystem` functions
        _module.args.pkgs = inputs'.nixpkgs.legacyPackages;
        # make custom lib available to all `perSystem` functions
        _module.args.lib = lib;
      };
    };
}
