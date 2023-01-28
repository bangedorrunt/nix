{ ... }:
{
  imports = [
    ./i18n.nix
    ./minimal-docs.nix
    ./openssh.nix
    ./server.nix
    ./tailscale.nix
  ];
}
