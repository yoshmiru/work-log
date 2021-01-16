
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "fuho";
  buildInputs = [ ag entr gnumake stack elmPackages.elm ];

  shellHook = ''
    ag -l -G '\.hs$|\.cabal$|\.elm$|.yaml$' | entr sh -c 'make build'
  '';
}
