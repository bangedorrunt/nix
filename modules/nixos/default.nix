{ config, pkgs, ... }: {
  # TODO generate minimal nixOS configuration
  time.timeZone = "Australia/Melbourne";

  # Passwordless accounts, auth is done via keypair
  nix.settings.trusted-users = [ "@wheel" ];
  security.sudo.wheelNeedsPassword = false;

  # Add user
  my.user = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = mySSHKeys;
  };

  # OpenSSH configuration
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    permitRootLogin = "no";
  };
  programs.mosh.enable = true;

  services.tailscale.enable = true;

  # Garbage collect nix stores weekly
  nix.gc = {
    automatic = true;
    dates = "Sunday 04:00";
    options = "--delete-older-than 7d";
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
