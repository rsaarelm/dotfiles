{ lib, stdenv, pkgs, ... }:

let
  dosbox-debug = pkgs.dosbox.overrideAttrs(attr: {
    configureFlags = [ "--disable-sdltest" "--enable-debug=heavy" ];

  buildInputs = with pkgs; [
      SDL
      SDL_net
      SDL_sound
      libGL
      libGLU
      libpng
      ncurses
    ];
  });
in
{
  home.packages = [
    dosbox-debug
  ];
}

