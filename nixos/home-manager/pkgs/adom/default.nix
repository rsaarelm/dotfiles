{ lib, stdenv, fetchurl, patchelf, zlib, ncurses5 }:

let

  lpath = "${stdenv.cc.cc.lib}/lib64:"
    + lib.makeLibraryPath [ zlib ncurses5 ];

in stdenv.mkDerivation rec {
  name = "adom-${version}";
  version = "3.3.3";

  src = fetchurl {
    url =
      "https://www.adom.de/home/download/current/adom_linux_debian_64_${version}.tar.gz";
    sha256 = "1pa8dpllrvljqk326h9a4msv8qb2g4rjjx16xxsksqw084732jmp";
  };

  buildCommand = ''
    . $stdenv/setup

    unpackPhase

    mkdir -pv $out/bin
    cp adom/adom $out/bin/adom

    ${patchelf}/bin/patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${lpath}" \
      $out/bin/adom
  '';

  meta = with lib; {
    description = "A rogue-like game with nice graphical interface";
    homepage = "http://adom.de/";
    license = licenses.unfreeRedistributable;
    maintainers = [ maintainers.smironov ];

    # Please, notify me (smironov) if you need the x86 version
    platforms = [ "x86_64-linux" ];
  };
}
