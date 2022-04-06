{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc884
, stack ? pkgs.stack
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    stack
  ];

  shellHook = ''
  '';
}
