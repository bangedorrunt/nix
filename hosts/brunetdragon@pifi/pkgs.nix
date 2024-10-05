{
  pkgs,
  options,
  ...
}: {
  tdt.hm.packages = with pkgs; [
    neovim
    plex
    qbittorrent
    qbittorrent-nox
  ];
}
