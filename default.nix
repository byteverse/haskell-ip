{ package ? "ip", compiler ? "ghc844" }:

let
  nixpkgs = import ./nix/nixpkgs.nix {};
  pkgs = import nixpkgs { config = {}; };
  inherit (pkgs) haskell;

  filterPredicate = p: type:
    let path = baseNameOf p; in !(
         (type == "directory" && path == "dist")
      || (type == "symlink"   && path == "result")
      || (type == "directory" && path == ".git")
      || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
      || pkgs.lib.hasSuffix "~" path
      || pkgs.lib.hasSuffix ".o" path
      || pkgs.lib.hasSuffix ".so" path
      || pkgs.lib.hasSuffix ".nix" path);

  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {});
           build = name: path: self.callCabal2nixWithOptions name (builtins.filterSource filterPredicate path) "--benchmark" {};
         };
    {
      ip = build "ip" ./.;
      semirings = super.semirings_0_3_1_1;
    };
  };
in rec {
  ip = overrides.${package};
}
