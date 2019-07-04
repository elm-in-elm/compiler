let
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "nixOS";
    repo = "nixpkgs-channels";
    rev = "73392e79aa62e406683d6a732eb4f4101f4732be";
    sha256 = "049fq37sxdn5iis7ni407j2jsfrxbb41pgv5zawi02qj47f74az9";
  };
in with import nixpkgs {};
mkShell {
    buildInputs = with elmPackages; [
        nodejs-10_x
        elm elm-test elm-format elm-analyse
    ];
}
