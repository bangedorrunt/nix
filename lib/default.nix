{lib, ...} @ args:
with lib; let
  _lib = self: let
    _import = file: import file ({inherit self;} // args);
  in {
    attrs = _import ./attrs.nix;
    importers = _import ./importers.nix;
    options = _import ./options.nix;
    # BUG infinite recursion
    # inherit (self.attrs) mergeAny;
    # inherit (self.importers) rakeLeaves flattenTree;
    # inherit (self.options) mkEnableOpt' mkOpt mkOpt' mkOptStr mkBoolOpt;
  };
  mine = makeExtensible _lib;
in
  mine.extend (self: super:
    foldr (a: b: a // b) {} (attrValues super))
