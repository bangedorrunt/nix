# SEE https://github.com/NixOS/nixpkgs/blob/master/lib/fixed-points.nix
{lib, ...} @ args:
with lib; let
  _lib = self: let
    callLibs = file: import file ({lib = self;} // args);
  in {
    attrs = callLibs ./attrs.nix;
    importers = callLibs ./importers.nix;
    options = callLibs ./options.nix;
    # NOTE these are magically handled by `mine.extend` implementation below.
    # inherit (self.attrs) mergeAny;
    # inherit (self.importers) rakeLeaves flattenTree;
    # inherit (self.options) mkEnableOpt' mkOpt mkOpt' mkOptStr mkBoolOpt;
  };
  # NOTE `makeExtensible` allows `self` referencing
  mine = makeExtensible _lib;
in
  mine.extend (self: super:
    foldr (a: b: a // b) {} (attrValues super))
