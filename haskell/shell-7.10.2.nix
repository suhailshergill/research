with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc7102.callPackage ./. {}).env
