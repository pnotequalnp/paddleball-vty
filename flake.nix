{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    yampa-vty.url = "github:pnotequalnp/yampa-vty";
  };

  outputs = { self, nixpkgs, flake-utils, yampa-vty }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            final.lib.composeExtensions prev.haskell.packageOverrides
            (hsFinal: hsPrev: {
              paddleball-vty = hsFinal.callCabal2nix "paddleball-vty" ./. { };
            });
        };
      };
      dependencies = yampa-vty.overlays.default;
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay dependencies ];
        };
        inherit (pkgs.haskell.packages) ghc8107 ghc921;
        tools = with ghc921; [ cabal-install fourmolu hlint pkgs.nixfmt ];
        devShell = hs:
          hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ paddleball-vty ];
            nativeBuildInputs = tools ++ [ hs.haskell-language-server ];
          };
      in {
        packages = {
          ghc921 = ghc921.paddleball-vty;
          ghc8107 = ghc8107.paddleball-vty;
          default = ghc921.paddleball-vty;
        };

        apps = rec {
          paddleball-vty = ghc921.paddleball-vty;
          default = paddleball-vty;
        };

        devShells = {
          default = devShell ghc921;
          ghc921 = devShell ghc921;
          ghc8107 = devShell ghc8107;
          ci = pkgs.mkShell { nativeBuildInputs = tools; };
        };
      }) // {
        overlays = {
          default = overlay;
          inherit dependencies;
        };
      };
}
