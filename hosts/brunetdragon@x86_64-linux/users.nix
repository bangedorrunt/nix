{ pkgs, ... }: {
  my.user = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ64CUihU1k0lLvd8Tf26fNiWnlP0C/bhlOOrXu2LExu brunetdragon@imac"
    ];
  };
}
