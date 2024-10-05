{...}: {
  system.stateVersion = "20.09";
  tdt.user = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
    hashedPassword = "$6$HyxbRPYxZdcvvkA6$cVXPWbFlHOL2bqm20Q6aTVMlmzRNHT6p6K6TVD62pFqJtGICS4.v//Nilo0AEnxjZxyeHsQF7fyW6PLKNYrIZ0";
  };
  networking.hostName = "pifi";
  networking = {
    interfaces."wlan0".useDHCP = true;
    wireless = {
      interfaces = ["wlan0"];
      enable = true;
      networks = {
        "StellanIsHere".psk = "00k5qfu3";
      };
    };
  };
  services.openssh.enable = true;
  programs.fish.enable = true;
}
