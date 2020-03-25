with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) {}
}:
with pkgs.haskell.lib;
let
  haskell-scope-src = pkgs.fetchFromGitHub {
    repo = "haskell-scope";
    owner = "haskell-suite";
    rev = "dd5544f080b0beca97312ac0d4ad8f332e0d223e";
    sha256 = "1rj2qiw4r0nzm87q5bh00jk5s0n4v02dzw9f4ana8jiwy7yj5gz3";
  };
  haskell-tc-src = pkgs.fetchFromGitHub {
    repo = "haskell-tc";
    owner = "haskell-suite";
    rev = "abf5e3cc624e0095cae14c3c1c695b76ea8ffcb9";
    sha256 = "1c0ncxazg9qagfnsn9nbvxs5xfh54xhk5rhxrsksax6m6pwiwv9p";
  };
  haskell-packages-src = pkgs.fetchFromGitHub {
    repo = "haskell-packages";
    owner = "haskell-suite";
    rev = "9131a0b737e352393e94351dd63c495f2e9ff3e6";
    sha256 = "14rdmp980p0wm69pry2bi1hs6asmsr02rqp3pqscmbg2fg1a2a73";
  };
  llvm-hs-pretty-src = pkgs.fetchFromGitHub {
    repo = "llvm-hs-pretty";
    owner = "llvm-hs";
    rev = "9df9ca533702ba104f2d475afeebb023b563c4ed";
    sha256 = "02dhri9z3vkg14b7dd1d0ag39a8vn3bvp7w5095k2lvxaj6vksq0";
  };
  llvm-hs-src = pkgs.fetchFromGitHub {
    repo = "llvm-hs";
    owner = "llvm-hs";
    rev = "b3a6fd0a7308ebf5fd820bc53106c7d8e98ea472";
    sha256 = "02x9plwn75n6na29why052vfq9irxnp85yn4y8jy8pwbkj8sy3gn";
  };
  overrides = self: super: {
    llvm_9 = (pkgs.llvm_9.override { debugVersion = true; }).overrideAttrs(_: { doCheck = false; });
    llvm-hs =
      let
        llvm-hs-pkg = self.callCabal2nix "llvm-hs" "${llvm-hs-src}/llvm-hs" { llvm-config = self.llvm_9; };
      in
        if pkgs.stdenv.isDarwin
          then
            overrideCabal llvm-hs-pkg (oldAttrs: {
               doCheck = false;
               preCompileBuildDriver = oldAttrs.preCompileBuildDriver or "" + ''
                 substituteInPlace Setup.hs --replace "addToLdLibraryPath libDir" "pure ()"
               '';
            })
          else
            llvm-hs-pkg;
    llvm-hs-pure = self.callCabal2nix "llvm-hs-pure"  "${llvm-hs-src}/llvm-hs-pure" {};
    llvm-hs-pretty = self.callCabal2nix "llvm-hs-pretty" llvm-hs-pretty-src {};
    tigr = self.callCabal2nix "tigr" ./tigr {};
    bedrock = dontHaddock (self.callCabal2nix "bedrock" ./bedrock {});
    libffi = dontHaddock (self.callCabal2nix "libffi" ./libffi {});
    haskell-crux = self.callCabal2nix "haskell-crux" ./haskell-crux {};
    haskell-scope = dontCheck (self.callCabal2nix "haskell-scope" haskell-scope-src {});
    haskell-packages = doJailbreak (self.callCabal2nix "haskell-packages" haskell-packages-src {});
    haskell-tc = dontHaddock (dontCheck (self.callCabal2nix "haskell-tc" haskell-tc-src {}));
  };
  hPkgs = pkgs.haskell.packages.ghc865.override { inherit overrides; };
  lhc = dontCheck (hPkgs.callCabal2nix "lhc" ./. {});
in
  lhc
