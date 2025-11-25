{
  description = "AGoC - Haskell WASM game with Playwright testing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcWasm = ghc-wasm-meta.packages.${system}.default;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghcWasm
            pkgs.nodejs
            pkgs.python3
          ];

          shellHook = ''
            # Install @playwright/test if not present
            if [ ! -d "node_modules/@playwright/test" ]; then
              echo "Installing @playwright/test..."
              npm install --save-dev @playwright/test
              npx playwright install chromium
            fi

            echo "GHC WASM and Playwright environment ready"
          '';
        };
      }
    );
}
