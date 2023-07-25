{
  description = "my project description";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.vivado20192.url = "github:euphgh/nur-vivado-2019.2";

  outputs = { self, nixpkgs, flake-utils, vivado20192, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = with pkgs; mkShell {
            package = with pkgs; [
              gnumake
              mill
              vivado20192.packages.${system}.default
            ];
            NIX_LD_LIBRARY_PATH = lib.makeLibraryPath [
              stdenv.cc.cc
              zlib
              ncurses6
            ];
            NIX_LD = lib.fileContents "${stdenv.cc}/nix-support/dynamic-linker";
          };
        }
      );
}
