{

  inputs = {
    miso.url = "github:dmjio/miso";
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.miso.inputs.nixpkgs.legacyPackages.${system};
        misoDevShells = inputs.miso.devShells.${system};
        ghcjsCompiler = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9141.ghc;
        playwrightNodeModule = pkgs.runCommand "playwright-node-module" {} ''
          mkdir -p $out
          ln -s ${pkgs.playwright-driver} $out/playwright
        '';
        testScript = pkgs.writeShellScriptBin "run-tests" (builtins.readFile ./scripts/run-tests);
        mkShell = shellInputs: withGhcjs:
          pkgs.mkShell {
            inputsFrom = shellInputs;
            packages = [ pkgs.zlib testScript ];
            shellHook = ''
              export MISO=${inputs.miso}
              export NODE_PATH=${playwrightNodeModule}:''${NODE_PATH:-}
              export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
            '' + pkgs.lib.optionalString withGhcjs ''
              export PATH=${ghcjsCompiler}/bin:$PATH
            '';
          };
      in
      {
        devShells.native = mkShell [ misoDevShells.default ] false;
        devShells.wasm = mkShell [ misoDevShells.default misoDevShells.wasm ] false;
        devShells.ghcjs = mkShell [ misoDevShells.default misoDevShells.ghcjs ] true;
        devShell = mkShell [
          misoDevShells.default
          misoDevShells.wasm
          misoDevShells.ghcjs
        ] true;
      }
    );

}

