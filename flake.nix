{
  inputs = {
    "nixos-23.05".url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "hex-text";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-23.05" = import inputs."nixos-23.05" { inherit system; };
        };
        pkgs = nixpkgs."nixos-23.05";
        project = pkgs.haskellPackages.developPackage {
          root = ./hex-text;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "96" ];
        };

        testConfigurations =
          let

            inherit (pkgs.haskell.lib) dontCheck;
            makeTestConfiguration =
              let defaultPkgs = pkgs;
              in { pkgs ? defaultPkgs, ghcVersion }:
                let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
                in (pkgs.haskell.packages.${ghcVersion}.override (old: {
                  overrides = (packageSourceOverrides { hex-text = ./hex-text; });
                })).hex-text;
          in
          rec {
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-23.05";
              ghcVersion = "ghc92";
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-23.05";
              ghcVersion = "ghc94";
            };
            ghc-9-6 = makeTestConfiguration {
              ghcVersion = "ghc96";
              pkgs = nixpkgs."nixos-23.05";
            };
            all = pkgs.symlinkJoin {
              name = packageName;
              paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ];
            };
          };
      in
      {
        defaultPackage = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          inputsFrom = [ testConfigurations.ghc-9-6.env ];
          buildInputs = [
            hls
            pkgs.cabal-install
          ];
        };

        packages = {
          "${packageName}" = project;
          inherit testConfigurations;
        };
      });
}
