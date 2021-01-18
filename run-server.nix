
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "fuho";
  buildInputs = [ gnumake stack elmPackages.elm ];

  shellHook = ''
    make client-build
    make server-start
  '';
}
