import { basename, dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';

const [target, clientPath, misoPath, jsffiPath] = process.argv.slice(2);
const baseUrl = 'http://127.0.0.1:8090/__client_test__';
const shimDir = fileURLToPath(new URL('../node_modules/@bjorn3/browser_wasi_shim/dist/', import.meta.url));
const assets = new Map([
  ['miso.js', misoPath],
  [target === 'wasm' ? 'client.wasm' : 'client.js', clientPath],
  ['ghc_wasm_jsffi.mjs', jsffiPath],
]);

if (!['wasm', 'ghcjs'].includes(target)) throw new Error(`Unknown client target: ${target}`);

const browser = await chromium.launch({
  headless: true,
  executablePath: process.env.CHROMIUM_BIN,
  args: ['--no-sandbox'],
});
let timeout;
try {
  const page = await browser.newPage();
  let succeed;
  let fail;
  const completed = new Promise((resolve, reject) => {
    succeed = resolve;
    fail = reject;
  });
  completed.catch(() => {});
  timeout = setTimeout(() => fail(new Error(`${target} browser client did not finish`)), 30000);
  page.on('console', message => {
    const text = message.text();
    if (text === 'Failed to load resource: the server responded with a status of 503 (Service Unavailable)'
      || text === 'Failed to load resource: net::ERR_CONNECTION_REFUSED') return;
    console.log(`[${target} browser] ${text}`);
    if (text === 'SUCCESS') succeed();
    if (text.startsWith('ERROR:')) fail(new Error(text));
  });
  page.on('pageerror', fail);
  await page.route(`${baseUrl}/**`, async route => {
    const name = new URL(route.request().url()).pathname.slice('/__client_test__/'.length);
    if (name === 'index.html') {
      await route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Client tests</title>' });
      return;
    }
    const path = name.startsWith('shim/')
      ? join(shimDir, basename(name))
      : assets.get(name);
    if (!path) {
      await route.fulfill({ status: 404, body: 'Unknown test asset' });
      return;
    }
    await route.fulfill({ path, contentType: name.endsWith('.wasm') ? 'application/wasm' : 'text/javascript' });
  });
  await page.addInitScript(() => {
    const networkFetch = globalThis.fetch.bind(globalThis);
    globalThis.fetch = (url, options) => new URL(url, location.href).pathname === '/malformed-json'
      ? Promise.resolve(new Response('not-json', {
        status: 200,
        headers: { 'content-type': 'application/json' },
      }))
      : networkFetch(url, options);
  });
  await page.goto(`${baseUrl}/index.html`);
  await page.addScriptTag({ url: `${baseUrl}/miso.js` });
  if (target === 'ghcjs') {
    await page.addScriptTag({ url: `${baseUrl}/client.js` });
    await completed;
  } else {
    await Promise.all([
      completed,
      page.evaluate(async () => {
        const { WASI, OpenFile, File, ConsoleStdout } = await import('/__client_test__/shim/index.js');
        const { default: jsffi } = await import('/__client_test__/ghc_wasm_jsffi.mjs');
        const wasi = new WASI(['client'], [], [
          new OpenFile(new File([])),
          ConsoleStdout.lineBuffered(message => console.log(message)),
          ConsoleStdout.lineBuffered(message => console.error(message)),
        ], { debug: false });
        const exports = {};
        const { instance } = await WebAssembly.instantiateStreaming(fetch('/__client_test__/client.wasm'), {
          wasi_snapshot_preview1: wasi.wasiImport,
          ghc_wasm_jsffi: jsffi(exports),
        });
        Object.assign(exports, instance.exports);
        wasi.initialize(instance);
        await instance.exports.hs_start();
      }),
    ]);
  }
} finally {
  clearTimeout(timeout);
  await browser.close();
}