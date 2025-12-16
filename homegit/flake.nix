{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";

    # TODO: Go back to using regular neovim once v0.12 is available from
    # stable NixOS. Remove the neovim package overrides from below and the
    # nightly overlay url here.
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      neovim-nightly,
    }:
    {
      nixosConfigurations.tungsten = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/tungsten/configuration.nix

          {
            programs.neovim.package = neovim-nightly.packages.x86_64-linux.default;
          }
        ];
      };

      nixosConfigurations.tantalum = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/tantalum/configuration.nix

          {
            programs.neovim.package = neovim-nightly.packages.x86_64-linux.default;
          }
        ];
      };
    };
}
