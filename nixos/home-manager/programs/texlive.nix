# Latex setup. This installs lots of packages.
{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      (texlive.combine {
        inherit (texlive)
          collection-basic metafont xits collection-bibtexextra
          collection-binextra collection-context collection-formatsextra
          collection-fontutils collection-langenglish collection-latex
          collection-latexextra collection-latexrecommended collection-pictures
          collection-pstricks collection-xetex;
      })
    ];
}

