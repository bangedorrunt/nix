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
    stable.url = "github:nixos/nixpkgs/nixos-24.05";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # system management
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;
    nixos-generators.url = "github:Mic92/nixos-generators/fedf7136f27490402fe8ab93e67fafae80513e9b";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.inputs.utils.follows = "flake-utils";
    # shell stuff
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    mission-control.url = "github:Platonic-Systems/mission-control";
    # editors
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    home-manager,
    flake-parts,
    flake-root,
    treefmt-nix,
    devshell,
    mission-control,
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
        devshell.flakeModule
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
          neovim = inputs.neovim-nightly.overlays.default;
        };
      };
      # REVIEW flake-part steal `self`, drop when this is fixed
      # _module.args._inputs = inputs // {inherit self;};

      # NOTE: for system and home configs, you do not use perSystem, full stop.
      # it is used only for things that are system specific, but should be made
      # available for any system in systems
      perSystem = {inputs', ...}: {
        # make pkgs available to all `perSystem` functions
        # this `legacyPackages` set is automatically created by
        # the `easyOverlay` module and include our overlay
        _module.args.pkgs = inputs'.nixpkgs.legacyPackages;
        # make custom lib available to all `perSystem` functions
        _module.args.lib = lib;
      };
    };
}
