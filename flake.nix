{
  description = "mnemonipass";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      gambitStatic = import ./nix/gambit-scheme.nix { inherit pkgs; };
    in {
      defaultPackage.${system} = self.packages.${system}.mnemonipass;
      packages.${system}.mnemonipass =
        import ./nix/mnemonipass.nix { inherit pkgs; gambit = gambitStatic; };
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          gambitStatic
          glibc
          glibc.static
        ];
      };
    };
}
