let
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz;
    sha256 = "04g53i02hrdfa6kcla5h1q3j50mx39fchva7z7l32pk699nla4hi";
  }) {};
  pkgs1909 = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }) {};
  haskellStuff = import (import ./sources.nix) { inherit pkgs; };
in
  pkgs.lib.overrideDerivation haskellStuff.env (old: {
    name = "habitica-party-chat";
    buildInputs = old.buildInputs ++ [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs1909.haskellPackages.stylish-haskell # broken in 20.03-beta
      pkgs.haskellPackages.hlint
      pkgs.spago
    ];
  })
