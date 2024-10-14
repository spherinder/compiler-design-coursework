{
  inputs = {};

  outputs =
    { systems, nixpkgs, ... }@inputs:
    let
      eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gnumake
            clang
            ocaml-ng.ocamlPackages_4_13.ocaml
            ocaml-ng.ocamlPackages_4_13.ocamlbuild
            ocaml-ng.ocamlPackages_4_13.menhir
            ocaml-ng.ocamlPackages_4_13.num
            # ocaml-ng.ocamlPackages_4_13.findlib
            # ocaml-ng.ocamlPackages_4_13.merlin
            ocaml-ng.ocamlPackages_4_13.ocp-indent
            ocaml-ng.ocamlPackages_4_13.ocamlformat
            ocaml-ng.ocamlPackages_4_13.utop
            # ocamlPackages.merlin
            # ocamlPackages.ocp-indent
            # ocamlPackages.ocamlformat
            # opam
          ];
        };
      });
    };
}
