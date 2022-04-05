{
  description = "Generate a PureScript API client for you servant API";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.purescript-bridge = {
    url = "github:mlabs-haskell/purescript-bridge";
    flake = false;
  };
  inputs.easy-ps = {
    url = "github:justinwoo/easy-purescript-nix";
    flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, easy-ps, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          servant-purescript =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
            } // {"purescript-bridge" = inputs.purescript-bridge;};
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; } // {"purescript-bridge" = inputs.purescript-bridge;} ;
      flake = pkgs.servant-purescript.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."servant-purescript:lib:servant-purescript" // {"purescript-bridge" = inputs.purescript-bridge;};
      devShell = pkgs.servant-purescript.shellFor {
        withHoogle = true;
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };
        exactDeps = true;
        buildInputs = with pkgs // {"purescript-bridge" = inputs.purescript-bridge;} ; with import easy-ps { inherit pkgs ; } // {"purescript-bridge" = inputs.purescript-bridge;}; [
          ghcid
          nixpkgs-fmt
          purs
          purescript-language-server
          spago
        ];
      };
    });
}