# TODO learn more about devshell
{...}: {
  perSystem = {
    pkgs,
    config,
    ...
  }: {
    devShells.default = pkgs.mkShell {
      inputsFrom = [config.mission-control.devShell];
      name = "setting-up-machines-nix-style";
      packages = with pkgs; [
        # Add your custom packages
        alejandra
        pre-commit
        nixd
        mkpasswd
      ];
    };
  };
}
