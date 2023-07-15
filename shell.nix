{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc902
, stack ? pkgs.stack
}:
pkgs.mkShell rec {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

  buildInputs = with pkgs; [
    ghc
    hlint
    stack
    zlib
  ];

  shellHook = ''
    chmod go-w . .ghci
  '';
}
