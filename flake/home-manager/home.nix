{ config, pkgs, inputs, lib, ... }:

{
  home.username = "rsaarelm";
  home.homeDirectory = "/home/rsaarelm";
  home.stateVersion = "22.11";

  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreePredicate = (_: true);
  };

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    bat
    exa
    fzf
    jq
    ripgrep
    tree
  ];

  programs.neovim = {
    enable.true;
    viAlias = true;
    vimAlias = true;
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  home.shellAliases = {
    l = "exa";
    ls = "exa";
    cat = "bat";
  };

  programs.zsh.enable = true;
}
