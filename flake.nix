{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "9122" ]; })
          pkgs.haskell.compiler.ghc9122
          pkgs.cabal-install
          pkgs.ormolu

          pkgs.watchexec
          pkgs.libzip
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
