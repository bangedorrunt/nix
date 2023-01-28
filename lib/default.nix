# TODO add `{user|host|utils}.nix`

# lib/default.nix -- This is used to extend nixpkgs' lib set to include
# some functions defined in lib/ that I used throughout my config.

# inputs.nixpkgs.lib is passed in here
lib:

rec {
  attrs = import ./attrs.nix lib;
  importers = import ./importers.nix lib;
  options = import ./options.nix lib;
  inherit (attrs) mergeAny;
  inherit (importers) rakeLeaves flattenTree;
}
