with import <nixpkgs> {};

stdenv.mkDerivation {
  src = "./.";
  name = "parsing-edsl";

  shellHook = ''
    exec ghci Code.hs
  '';

  buildInputs = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      vinyl
    ]) )
  ];
}
