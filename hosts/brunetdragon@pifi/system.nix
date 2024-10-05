{...}: {
  system.stateVersion = "20.09";

  raspberry-pi-nix.board = "bcm2712";

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
  hardware = {
    bluetooth.enable = true;
    raspberry-pi = {
      config = {
        all = {
          base-dt-params = {
            # enable autoprobing of bluetooth driver
            # https://github.com/raspberrypi/linux/blob/c8c99191e1419062ac8b668956d19e788865912a/arch/arm/boot/dts/overlays/README#L222-L224
            krnbt = {
              enable = true;
              value = "on";
            };
          };
        };
      };
    };
  };
  services.openssh.enable = true;
  programs.fish.enable = true;
}
