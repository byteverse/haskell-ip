{ package ? "ip", compiler ? "ghc844" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "45456fecc74f70476c583d4144019a1a9f48267e";
    sha256 = "1pp22lxh70qg9l5ylhc8bc89pvccg3xwiyi6q5qkqpnwbb2jcjmr";
  };
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
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {};
         };
    {
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = pkgs.lib.elem args.pname [ "ip" ];
        doHaddock = false;
      });

      ip = build "ip" ./.;
    };
  };
in rec {
  drv = overrides.${package};
  ip  = if pkgs.lib.inNixShell then drv.env else drv;
}
