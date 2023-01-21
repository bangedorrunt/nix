{ inputs, nixpkgs, stable, ... }: {
  nixpkgs.overlays = [
    (
      final: prev: {
        # Expose stable packages via pkgs.stable
        stable = import stable { system = prev.system; };
      }
    )
    # inputs.neovim-nightly-overlay.overlay
    (import ../modules/pkgs/sumneko-overlay.nix)
  ];
}
