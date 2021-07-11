{ config, lib, pkgs, inputs, ... }: {
  nixpkgs.overlays = [
    (final: prev: {
      neuron = (prev.callPackage "${inputs.neuron}/project.nix" { }).neuron;
    })
  ];
}
