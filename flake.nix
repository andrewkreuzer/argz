{
  description = "argz";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    zig.url          = "github:mitchellh/zig-overlay";
  };

  outputs = { nixpkgs, flake-utils, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (final: prev: {
            zig = inputs.zig.packages.${prev.system}."master";
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            zig
          ];
        };
      }
    );
}
