{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    with nixpkgs.legacyPackages.x86_64-linux;
    let
      system = "x86_64-linux";

      cabal = nixpkgs.legacyPackages.${system}.haskellPackages.callPackage ./cabal.nix {};
    in
      {
        devShells.${system}.default =
          mkShell {
            inputsFrom = [
              cabal.env
            ];

            buildInputs = [
              cabal-install
              cabal2nix
              ghc
              haskell-language-server
              gnuplot
            ];
          };
      };
}
