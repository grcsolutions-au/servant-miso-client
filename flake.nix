{

  inputs = {
    miso.url = "github:dmjio/miso";
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.miso.inputs.nixpkgs.legacyPackages.${system};
        misoDevShells = inputs.miso.devShells.${system};
        testScript = pkgs.writeShellScriptBin "run-tests" (builtins.readFile ./scripts/run-tests);
        mkShell = shellInputs:
          pkgs.mkShell {
            inputsFrom = shellInputs;
            packages = [ pkgs.zlib testScript ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.chromium ];
            shellHook = ''
              export MISO=${inputs.miso}
            '';
          };
      in
      {
        devShells.native = mkShell [ misoDevShells.default ];
        devShells.wasm = mkShell [ misoDevShells.default misoDevShells.wasm ];
        devShells.ghcjs = mkShell [ misoDevShells.default misoDevShells.ghcjs ];
        devShell = mkShell [
          misoDevShells.default
          misoDevShells.wasm
          misoDevShells.ghcjs
        ];
      }
    );

}

