# as of 2019-06-30, this must be run with unstable channel
# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs 
# nix-channel --update

with import <nixpkgs>{};
stdenv.mkDerivation rec {
    name = "elm-in-elm";
    buildInputs = with elmPackages; [
        nodejs-10_x
        elm elm-test elm-format elm-analyse
    ];
}
