#!/usr/bin/env bash
set -euo pipefail

# Build artifacts, vendor browser deps, and run Playwright E2E tests.

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$root_dir"

echo "==> Building WASM and copying artifacts to public/ (./locally.sh)"
nix develop -c ./locally.sh

echo "==> Vendoring browser dependencies (public/vendor/*)"
nix develop -c ./scripts/vendor-dependencies.sh

echo "==> Running Playwright E2E tests"
PWTEST_TIMEOUT=45000 PWTEST_WORKERS=1 nix develop -c npx playwright test --timeout=45000 --workers=1 --retries=1 "$@"

echo "==> Playwright tests completed"

