#!/usr/bin/env bash
set -euo pipefail

# Run all local tests using the standard nix dev shell tools.
# This script assumes `nix develop` is available (flake).

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$root_dir"

echo "==> Building WASM and copying artifacts to public/ (./locally.sh)"
nix develop -c ./locally.sh

echo "==> Vendoring browser dependencies (public/vendor/*)"
nix develop -c ./scripts/vendor-dependencies.sh

echo "==> Building test suite wasm with wasm32-wasi-cabal"
nix develop -c wasm32-wasi-cabal build agoc:agoc-tests

# Locate the produced test wasm
TEST_WASM="$(find dist-newstyle -path '*agoc-tests.wasm' | head -n1)"
if [ -z "$TEST_WASM" ]; then
  echo "ERROR: agoc-tests.wasm not found in dist-newstyle"
  exit 1
fi

echo "==> Copying test wasm locally"
cp "$TEST_WASM" ./agoc-tests.wasm

echo "==> Generating JS glue with post-link.mjs"
# Grab libdir cleanly (strip shell hook noise)
POST_LINK="$(nix develop --command wasm32-wasi-ghc --print-libdir | tail -n1)/post-link.mjs"
nix develop -c node "$POST_LINK" -i ./agoc-tests.wasm -o ./agoc-tests.mjs

echo "==> Running test wasm with node + WASI + ghc_wasm_jsffi imports"
nix develop -c node --input-type=module <<'EOF'
import fs from 'fs';
import path from 'path';
import { WASI } from 'wasi';
import { fileURLToPath } from 'url';

const wasmBytes = fs.readFileSync('./agoc-tests.wasm');
const wasi = new WASI({
  version: 'preview1',
  args: ['/agoc-tests'],
  env: { ...process.env, PWD: '/' },
  preopens: { '/': process.cwd() }
});

const __exports = {};
const jsffiMod = await import(path.resolve('./agoc-tests.mjs'));
const imports = {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: jsffiMod.default(__exports)
};

const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
Object.assign(__exports, instance.exports);
if (__exports._start) {
  // Command-style module
  wasi.start(instance);
} else {
  // Reactor-style module
  wasi.initialize(instance);
  if (typeof __exports.main === 'function') {
    __exports.main();
  } else if (typeof __exports.hs_main === 'function') {
    const r = __exports.hs_main();
    if (r?.then) await r;
  } else if (typeof __exports._initialize === 'function') {
    __exports._initialize();
  } else {
    throw new Error('No entrypoint (_start/main/hs_main/_initialize) found in agoc-tests.wasm');
  }
}
EOF

echo "==> Running Playwright E2E tests"
nix develop -c npx playwright test

echo "==> All tests completed"

