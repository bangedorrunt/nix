{ inputs, config, lib, pkgs, ... }: {
  imports = [ ./primary.nix ./nixpkgs.nix ./overlays.nix ];

  /* programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
  }; */

  user = {
    description = "Thanh Dung TRUONG";
    home = "${
        if pkgs.stdenvNoCC.isDarwin then "/Users" else "/home"
      }/${config.user.name}";
    shell = pkgs.zsh;
  };

  # Bootstrap Home Manager using system config
  hm = import ./home-manager;

  # Let Nix manage Home-Manager profiles and use global nixpkgs
  home-manager = {
    extraSpecialArgs = { inherit inputs lib; };
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
  };

  # Environment setup
  environment = {
    systemPackages = with pkgs; [
      # Editors
      neovim-nightly

      # Standard toolset
      coreutils
      curl
      wget
      git
      jq

      # Helpful shell stuff
      bat
      fzf
      ripgrep
      zsh

      # Languages
      python3
    ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${inputs.nixpkgs}";
    };
    # List of acceptable shells in /etc/shells
    shells = with pkgs; [ bash zsh fish ];
  };

  /* fonts.fonts = with pkgs; [ jetbrains-mono iosevka ]; */
}
