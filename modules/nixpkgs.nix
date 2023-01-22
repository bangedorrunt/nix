{ self, inputs, config, lib, pkgs, ... }: {
  nixpkgs = {
    config = import ./config.nix;
    overlays = builtins.attrValues self.overlays;
  };

  nix = {
    package = pkgs.nix;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    settings = {
      max-jobs = 8;
      trusted-users = [ "${config.my.username}" "root" "@admin" "@wheel" ];
      trusted-substituters = [
        "https://bangedorrunt.cachix.org"
        "https://cachix.cachix.org"
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://cachix.org/api/v1/cache/emacs"
      ];
      trusted-public-keys = [
        "bangedorrunt.cachix.org-1:5SFYJPXVbo9clgdN+2C8T6bJUYh1WOLKsPzzIsOOWyA="
        "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
      ];
    };
    readOnlyStore = true;
    nixPath = builtins.map
      (source: "${source}=/etc/${config.environment.etc.${source}.target}") [
      "home-manager"
      "nixpkgs"
      "stable"
    ];
    registry = {
      nixpkgs = {
        from = {
          id = "nixpkgs";
          type = "indirect";
        };
        flake = inputs.nixpkgs;
      };

      stable = {
        from = {
          id = "stable";
          type = "indirect";
        };
        flake = inputs.stable;
      };
    };
  };
}
