{
  description = "Home Manager configuration of volod";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # https://github.com/kachick/dotfiles/blob/main/flake.nix

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }: {
    homeConfigurations = {
      "ubuntu@ubuntu" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-linux;
          modules = [ ./home-rpi5.nix ];
      };
    };

    defaultPackage = {
      aarch64-linux = home-manager.defaultPackage.aarch64-linux;
    };

  };
}
