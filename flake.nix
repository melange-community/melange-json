{
  description = "melange-json Nix Flake";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs = {
    url = "github:nix-ocaml/nix-overlays";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".appendOverlays [
          (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_2;
          })
        ];
        inherit (pkgs) nodejs_latest lib stdenv darwin;

        melange-json = with pkgs.ocamlPackages; buildDunePackage {
          pname = "melange-json";
          version = "dev";

          src = ./.;
          nativeBuildInputs = with pkgs.ocamlPackages; [ melange ];
          propagatedBuildInputs = with pkgs.ocamlPackages; [
            melange
            yojson
            ppxlib
          ];
        };

        mkShell = { buildInputs ? [ ] }: pkgs.mkShell {
          inputsFrom = [ melange-json ];
          nativeBuildInputs = with pkgs; [
            yarn
            nodejs_latest
          ] ++ (with pkgs.ocamlPackages; [
            ocamlformat
            merlin
            melange-jest
            reason
          ]);
          inherit buildInputs;
        };
      in
      rec {
        packages.default = melange-json;
        devShells = {
          default = mkShell { };
          release = mkShell {
            buildInputs = with pkgs; [ cacert curl ocamlPackages.dune-release git ];
          };
        };
      });
}
