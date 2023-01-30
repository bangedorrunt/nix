{
  perSystem = {
    config,
    pkgs,
    lib,
    ...
  }: {
    treefmt.config = {
      inherit (config.flake-root) projectRootFile;
      package = pkgs.treefmt;

      programs = {
        alejandra.enable = true;
        black.enable = true;
        gofmt.enable = true;
        prettier.enable = true;
        rufo.enable = true;
        shellcheck.enable = false;
        shfmt.enable = true;
        stylua.enable = true;
      };

      # settings.formatter = {
      #   stylua.options = ["--indent-type" "Spaces"];
      # };
    };

    formatter = config.treefmt.build.wrapper;
  };
}
