{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations.tungsten = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./hosts/tungsten/configuration.nix ];
    };

    nixosConfigurations.tantalum = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./hosts/tantalum/configuration.nix ];
    };
  };
}
