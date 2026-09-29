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
        browserWasiShim = pkgs.stdenvNoCC.mkDerivation {
          pname = "browser-wasi-shim";
          version = "0.3.0";
          src = pkgs.fetchurl {
            url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.3.0.tgz";
            hash = "sha512-FlRBYttPRLcWORzBe6g8nmYTafBkOEFeOqMYM4tAHJzFsQy4+xJA94z85a9BCs8S+Uzfh9LrkpII7DXr2iUVFg==";
          };
          nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
          dontUnpack = true;
          dontBuild = true;
          installPhase = ''
            mkdir -p $out
            tar -xzf $src --strip-components=1 -C $out
          '';
        };
        testScript = pkgs.writeShellScriptBin "run-tests" (builtins.readFile ./scripts/run-tests);
        mkShell = shellInputs: withGhcjs:
          pkgs.mkShell {
            inputsFrom = shellInputs;
            packages = [ pkgs.zlib pkgs.bun pkgs.nodejs pkgs.http-server browserWasiShim testScript ];
            shellHook = ''
              export MISO=${inputs.miso}
              export NODE_PATH=${playwrightNodeModule}:''${NODE_PATH:-}
              export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
              export BROWSER_WASI_SHIM=${browserWasiShim}/dist
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
        packages.browser-wasi-shim = browserWasiShim;
      }
    );

}

