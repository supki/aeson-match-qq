{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc984
}:
pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    stack
    zlib
  ];

  shellHook = ''
    export \
      LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}
    chmod go-w . .ghci
  '';
}
