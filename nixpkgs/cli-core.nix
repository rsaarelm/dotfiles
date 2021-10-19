# Essential CLI packages
{ ... }:

{
  imports = [
    ./programs/git.nix
    ./programs/neovim.nix
    ./programs/zsh.nix
  ] ++ (if builtins.pathExists ./local.nix then [ ./local.nix ] else [ ]);
  # Extra settings can be added in non-version-controlled local.nix

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;

    extraConfig = ''
      AddKeysToAgent yes
    '';
  };

  services.lorri.enable = true;
}