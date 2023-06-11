{
  description = "my project description";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = with pkgs; mkShell {
            package = with pkgs; [
              gnumake
              mill
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
