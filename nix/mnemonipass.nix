{
  pkgs,
  gambit,
}:

with pkgs;
stdenv.mkDerivation {
  pname = "mnemonipass";
  version = "0.0.1";
  src = ./..;
  makeFlags =
    [
      "PREFIX=$(out)"
    ];
  buildInputs = [
    glibc.static
    gambit
  ];
  postInstall = ''
    strip $out/bin/mnemonipass
  '';
}
