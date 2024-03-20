{
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";
  outputs = { self, nixpkgs, flake-utils }: (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    with pkgs; {
      devShells.default = mkShell {
        nativeBuildInputs = with ocamlPackages; [
          dune
          ocaml
          ocaml-lsp
          melange
          melange-webapi
          findlib
          ocamlformat
        ];
      };
    }));
}
