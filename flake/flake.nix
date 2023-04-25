{
  description = "System flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Home-manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Hardware
    hardware.url = "github:nixos/nixos-hardware";
    # Hyprland
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = { self, nixpkgs, home-manager, hyprland, ... }@inputs: {

    # nixos-rebuild --flake .#tantalum
    nixosConfigurations = {
      tantalum = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };

        modules = [
          ./nixos/configuration.nix
          hyprland.nixosModules.default
          {programs.hyprland.enable = true;}
        ];
      };
    };

    # home-manager --flake .#rsaarelm@tantalum
    homeConfigurations = {
      "rsaarelm@tantalum" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./home-manager/home.nix
        ];
      };
    };
  };
}
