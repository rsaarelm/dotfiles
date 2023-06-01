self: super: {
  apl385 = super.stdenv.mkDerivation {
    name = "apl385";
    src = super.fetchurl {
      url = "http://apl385.com/fonts/apl385.zip";
      sha256 = "132qfsnx0v6qf8x8iy3flivv449nz42nnpkwjysmz65w6wqxpk1g";
    };
    buildInputs = [ super.unzip ];
    sourceRoot = ".";
    installPhase = ''
      out1=$out/share/fonts/apl385
      mkdir -p $out1
      cp ./Apl385.ttf $out1
    '';
  };

  # TODO 2023-06-01 Drop local derivation for intel-one-mono when it's officially supported in NixOS
  intel-one-mono = super.stdenv.mkDerivation {
    name = "intel-one-mono";
    src = super.fetchurl {
      url = "https://github.com/intel/intel-one-mono/raw/main/fonts/ttf.zip";
      sha256 = "91acf40de3de3bc9ef295a2760986ce5fdbea6cff95e8e69b23835868c736a25";
    };
    buildInputs = [ super.unzip ];
    sourceRoot = ".";
    installPhase = ''
      out1=$out/share/fonts/intel-one-mono
      mkdir -p $out1
      cp ./ttf/*.ttf $out1
    '';
  };
}
