{ pkgs, ... }: {
  imports = [
    ./core.nix
    ./brew.nix
    ./preferences.nix
    ./yabai.nix
  ];
}
