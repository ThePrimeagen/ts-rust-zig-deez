{
  description = "Monkey Lang Interpreter";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/30f8cac903a4c968fa2d5d1c99dd8c67dd716456";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = self: super: {
        hsPkgs = super.haskell.packages.ghc944.override {
          overrides = hself: hsuper: {
            ghcid = super.haskell.lib.dontCheck hsuper.ghcid;
          };
        };
        haskell-src = self.hsPkgs.callCabal2nix "haskell-src" ./. { };
      };

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          haskell-src = pkgs.haskell-src;
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          libs = with pkgs; [
            zlib
          ];
        in
        {
          default = pkgs.hsPkgs.shellFor {
            packages = hsPkgs: [ ];
            buildInputs = with pkgs; [
              hsPkgs.cabal-install
              hsPkgs.cabal-fmt
              hsPkgs.ghcid
              hsPkgs.ghc
              ormolu
              nixpkgs-fmt
              hsPkgs.cabal-fmt
              treefmt
            ] ++ libs;
            shellHook = "export PS1='[$PWD]\n❄ '";
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          };
        });
    };
}
