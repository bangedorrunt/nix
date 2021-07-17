{ pkgs, ... }: {
  imports = [
    ./core.nix
    ./brew.nix
    ./preferences.nix
    ./yabai.nix
    ./goku.nix
  ];
  my.modules.goku.enable = true;
  my.modules.yabai.enable = true;
}
