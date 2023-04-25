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
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: {

    # nixos-rebuild --flake .#tantalum
    nixosConfiguration = {
      tantalum = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };

        modules = [
          ./nixos/configuration.nix
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
