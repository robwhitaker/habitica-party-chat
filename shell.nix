let
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/053ad4e0db7241ae6a02394d62750fdc5d64aa9f.tar.gz;
    sha256 = "11l9sr8zg8j1n5p43zjkqwpj59gn8c84z1kf16icnsbnv2smzqdc";
  }) {};
  haskellStuff = import (import ./sources.nix) { inherit pkgs; };
in
  pkgs.lib.overrideDerivation haskellStuff.env (old: {
    name = "habitica-party-chat";
    buildInputs = old.buildInputs ++ [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.hlint
      pkgs.spago
    ];
  })
