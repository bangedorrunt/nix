{
  self,
  config,
  pkgs,
  ...
}:
with builtins; {
  nixpkgs = {
    config = {
      allowUnsupportedSystem = true;
      allowUnfree = true;
      allowBroken = false;
    };
    overlays = attrValues self.overlays;
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
      options = "--delete-older-than 5d";
    };
    settings = {
      max-jobs = 8;
      trusted-users = ["${config.tdt.username}" "root" "@admin" "@wheel"];
      trusted-substituters = [
        "https://bangedorrunt.cachix.org"
        "https://cachix.cachix.org"
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
      ];
      extra-trusted-public-keys = [
        "bangedorrunt.cachix.org-1:5SFYJPXVbo9clgdN+2C8T6bJUYh1WOLKsPzzIsOOWyA="
        "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    nixPath =
      map
      (source: "${source}=/etc/${config.environment.etc.${source}.target}") [
        "home-manager"
        "nixpkgs"
        "stable"
      ];
  };
}
