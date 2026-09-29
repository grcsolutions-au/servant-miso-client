import { createRequire } from 'node:module';
import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

const [target, clientPath, misoPath, jsffiPath] = process.argv.slice(2);
const require = createRequire(import.meta.url);

try {
  require(misoPath);
  if (target === 'ghcjs') {
    const fetchFromNetwork = globalThis.fetch.bind(globalThis);
    globalThis.fetch = (...args) => fetchFromNetwork(...args).catch(error => {
      console.error('fetch rejected:', args[0], error.cause ?? error);
      throw error;
    });
    const originalLog = console.log;
    let timeout;
    try {
      await new Promise((resolve, reject) => {
        timeout = setTimeout(() => reject(new Error('GHCJS client did not finish')), 30000);
        console.log = (...args) => {
          originalLog(...args);
          if (args.length === 1 && args[0] === 'SUCCESS') resolve();
        };
        require(clientPath);
      });
    } finally {
      clearTimeout(timeout);
      console.log = originalLog;
    }
  } else if (target === 'wasm') {
    const { WASI } = await import('node:wasi');
    const wasi = new WASI({ version: 'preview1', args: [clientPath], env: {}, returnOnExit: true });
    const { default: jsffi } = await import(pathToFileURL(jsffiPath).href);
    const exports = {};
    const module = new WebAssembly.Module(readFileSync(clientPath));
    const instance = new WebAssembly.Instance(module, {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: jsffi(exports),
    });
    Object.assign(exports, instance.exports);
    wasi.initialize(instance);
    await instance.exports.hs_start();
  } else {
    throw new Error(`Unknown client target: ${target}`);
  }
} catch (error) {
  console.error('Browser client failed:', error, `(type: ${typeof error})`);
  process.exitCode = 1;
}