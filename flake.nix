{
  description = "Minimal Lean 4 flake.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs = { self, nixpkgs }@inputs:
    let
      system = "x86_64-linux";
      pkgs   = nixpkgs.legacyPackages.${system};
    in
      {
        packages.${system}.default = pkgs.buildLakePackage {
            pname   = "lean-calc";
            version = "1.0.0";
            src     = pkgs.lib.cleanSource ./.;

            lakeBuildFlages = [ ":lean-calc" ];

            meta = with pkgs.lib; {
              description = "A partial implementation of the \"ac\" language presented in \"Crafting a Compiler\".";
              license     = licenses.mit;
            };

            postInstall = ''
# Removing dot files
cd $out
rm -rf .envrc .direnv
'';
        };
        
        devShells.${system}.default = pkgs.mkShell {
          packages = with pkgs; [
            lean4
          ];
        };
      };
}
