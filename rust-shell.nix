with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-env";
  buildInputs = [
    rustup

    # Example Additional Dependencies
    pkgconfig openssl

    cmake gcc zlib
  ];

  # XXX: Not doing stuff in proper Nix way and just running the Nix spells.
  shellHook = ''
    rustup install stable
    rustup install nightly
    rustup update

    rustup component add rls-preview rust-analysis rust-src
    rustup component add rustfmt-preview clippy-preview --toolchain nightly

    # FIXME: These run into some linker problem when run from shellHook
    # They can be installed manually once in the shell though.
    # cargo install cargo-outdated
  '';

  # Set Environment Variables
  RUST_BACKTRACE = 1;

  # Stuff needed to run Glium programs.
  LD_LIBRARY_PATH = with pkgs.xlibs; "${pkgs.mesa}/lib:${libX11}/lib:${libXcursor}/lib:${libXxf86vm}/lib:${libXi}/lib:${libXrandr}/lib";
}
