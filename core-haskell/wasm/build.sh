#!/usr/bin/env bash
# Compila o núcleo Haskell para WebAssembly (WASI reactor + JSFFI) e gera o
# glue JS. Requer o toolchain wasm32-wasi-ghc do ghc-wasm-meta instalado no
# Linux/WSL2 (~/.ghc-wasm). Rode a partir de qualquer lugar: as saídas vão
# para public/gameoflife.{wasm,js} do frontend.
#
#   bash core-haskell/wasm/build.sh            # saída em ./public
#   bash core-haskell/wasm/build.sh /algum/dir # saída customizada
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"   # core-haskell/wasm
CORE="$(cd "$HERE/.." && pwd)"          # core-haskell
ROOT="$(cd "$CORE/.." && pwd)"          # raiz do repositório
OUT="${1:-$ROOT/public}"

if [ -f "$HOME/.ghc-wasm/env" ]; then
  # shellcheck disable=SC1091
  . "$HOME/.ghc-wasm/env"
fi

GHC="${WASM_GHC:-wasm32-wasi-ghc}"
command -v "$GHC" >/dev/null || { echo "erro: $GHC não encontrado (instale o ghc-wasm-meta)"; exit 1; }

LIBDIR="$("$GHC" --print-libdir)"
mkdir -p "$OUT"
OBJDIR="$(mktemp -d)"

cd "$CORE"

"$GHC" \
  -O2 \
  -no-hs-main \
  -outputdir "$OBJDIR" \
  -optl-mexec-model=reactor \
  -optl-Wl,--export=runSteps \
  wasm/GameOfLifeWasm.hs src/GameOfLife.hs src/GameOfLifeApi.hs \
  -o "$OUT/gameoflife.wasm"

# post-link.mjs transforma as seções ghc_wasm_jsffi do wasm no módulo JS
# que fornece os imports ghc_wasm_jsffi em tempo de instanciação.
node "$LIBDIR/post-link.mjs" -i "$OUT/gameoflife.wasm" -o "$OUT/gameoflife.js"

echo "gerado:"
ls -la "$OUT/gameoflife.wasm" "$OUT/gameoflife.js"
