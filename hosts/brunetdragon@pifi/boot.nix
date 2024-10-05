{...}: {
  # Configure boot
  boot = {
    # NOTE: if your build-machine isn't aarch64-linux,
    # then you will need to add the following to your
    # build machine's nixos configuration to emulate aarch64 (arm64)
    # so you can cross-build.
    binfmt.emulatedSystems = ["aarch64-linux"];
    # loader.systemd-boot.enable = true;
    # loader.efi.canTouchEfiVariables = true;
    supportedFilesystems = ["btrfs" "vfat" "ext4"];
  };
}
