{
  perSystem = {
    self',
    config,
    lib,
    pkgs,
    ...
  }: let
    flash-iso-image = name: image: let
      pv = "${pkgs.pv}/bin/pv";
      fzf = "${pkgs.fzf}/bin/fzf";
    in
      pkgs.writeShellScriptBin name ''
        set -euo pipefail

        # Build image
        nix build .#${image}

        # Display fzf disk selector
        iso="./result/iso/"
        iso="$iso$(ls "$iso" | ${pv})"
        dev="/dev/$(lsblk -d -n --output RM,NAME,FSTYPE,SIZE,LABEL,TYPE,VENDOR,UUID | awk '{ if ($1 == 1) { print } }' | ${fzf} | awk '{print $2}')"

        # Format
        ${pv} -tpreb "$iso" | sudo dd bs=4M of="$dev" iflag=fullblock conv=notrunc,noerror oflag=sync
      '';
  in {
    mission-control.scripts = {
      nix-build-darwin = {
        category = "Nix";
        description = "Builds Nix for macOS";
        exec = pkgs.writeShellScriptBin "nix-build-darwin" ''
          set -euo pipefail
          nix build .#darwinConfigurations."brunetdragon@x86_64-darwin".config.system.build.toplevel
        '';
      };
      nix-build-nixos = {
        category = "Nix";
        description = "Builds NixOS";
        exec = pkgs.writeShellScriptBin "nix-build-nixos" ''
          set -euo pipefail
          nix build .#nixosConfigurations."brunetdragon@x86_64-linux".config.system.build.toplevel
        '';
      };
      # ISOs
      flash-nixos-iso = {
        category = "Images";
        description = "Flash installer-iso image for NixOS";
        exec = flash-iso-image "flash-nixos-iso" "nixos-iso-image";
      };

      # Utils
      fmt = {
        category = "Dev Tools";
        description = "Format the source tree";
        exec = "${lib.getExe config.treefmt.build.wrapper}";
      };

      clean = {
        category = "Utils";
        description = "Cleans any result produced by Nix or associated tools";
        exec = pkgs.writeShellScriptBin "clean" "rm -rf result* *.qcow2";
      };

      run-vm = {
        category = "Utils";
        description = "Executes a VM if output derivation contains one";
        exec = "exec ./result/bin/run-*-vm";
      };
    };
  };
}
