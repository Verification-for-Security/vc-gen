let
  pin = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
in
{ pkgs ? import pin { config = {}; overlays = []; } }:

let
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --nix-packages z3 \
        "
    '';
  };

in
pkgs.mkShell {
  buildInputs = [
    pkgs.nix
    stack-wrapped
  ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
