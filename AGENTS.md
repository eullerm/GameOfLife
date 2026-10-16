# AGENTS.md

Two projects in one repo:

- **Frontend (repo root)**: Vite + React 19 + TypeScript. Entry: `src/main.tsx` -> `src/App.tsx`. Runs the Haskell core exclusively in the browser via WebAssembly (no HTTP), through `src/core/transport.ts`.
- **Haskell core (`core-haskell/`)**: stack project (`stack.yaml` resolver lts-18.18, package `GameOfLife.cabal`). One library (`GameOfLife` = game logic, `GameOfLifeApi` = pure wrapper) and two executables:
  - `GameOfLife` — the original interactive stdin/stdout console program (`core-haskell/src/Main.hs`);
  - `api` — local HTTP server exposing the core (`core-haskell/server/Main.hs`, port **8787**, override with `GOL_API_PORT`).
  - `core-haskell/wasm/` — `GameOfLifeWasm.hs` (WASI/JSFFI entry point) and `build.sh`, which compiles the same library to `public/gameoflife.{wasm,js}` for the browser.

## Commands (frontend)

- `npm run dev` — dev server (no proxy: the app only uses the WASM core)
- `npm run lint` — ESLint over the whole repo (no separate typecheck script)
- `npm run build` — `tsc -b && vite build`; this **is** the typecheck step
- `npm run preview`
- `npm run api` — `cd core-haskell && stack build && stack exec api` (builds + starts the backend)
- `npm run wasm` — `wsl -d Ubuntu -- bash core-haskell/wasm/build.sh` (rebuilds `public/gameoflife.{wasm,js}` from Haskell via GHC WASI; requires WSL2 toolchain)
- Verification order: `npm run lint` -> `npm run build`. No test framework exists (no test script, no vitest/jest).

Baseline is currently **green** for `npm run lint` and `npm run build`.

## Commands (Haskell)

- Run `stack build` from **inside `core-haskell/`** — `stack.yaml` lives there; there is no root `stack.yaml` anymore. First build downloads the LTS-18 toolchain (large/slow).
- `stack build` builds the library + both executables; console usage: `stack exec GameOfLife`; API: `stack exec api`.
- `stack.yaml` has `flags: mintty: Win32-2-13-1: false` — required on Windows because LTS-18.18 pins Win32-2.6.2.1 (< 2.13.1), and without the flag mintty's backport module is never compiled (build fails with "Could not find module System.Console.MinTTY.Win32").
- The `.vscode/tasks.json` "haskell test" task runs `stack test`, but the cabal file defines no test-suite — it will fail.
- `.vscode/*` tasks (F7 build, F6 clean+build, F8 test) were authored for the old root layout; they run `stack build` from the repo root and are stale.
- Port 8080 is often taken (e.g. by WSL's `wslrelay.exe` on Windows) — that is why the API uses 8787.

## How the two halves connect

- The frontend never imports Haskell directly; `src/core/transport.ts` defines `CoreTransport { health(); run(cells, steps) }` and only exports the WASM implementation (coreKind is fixed to `wasm`), running the Haskell core in the browser through `src/core/wasmCore.ts`. There is no HTTP transport and no `VITE_CORE`/`VITE_API_URL` switch.
- API contract: `GET /api/health` -> `ok`; `POST /api/generation` with `{cells: [[Int]], steps: Int}` -> `{cells, iterationsUntilStable, stabilized}` where `stabilized = iterationsUntilStable < steps`. Validation: rectangular grid 1..200 per side, cells 0..2, steps 0..10000; otherwise 400 with `{error: string}`.
- Cell states: `0 = Morta`, `1 = Viva`, `2 = Zumbi`. The core's rule priority lives in `aliveDeadOrZombie` (`core-haskell/src/GameOfLife.hs`): `V` with any zombie neighbour becomes `Z` before the `a<2`/`a>3` death rules.
- WASM build: `core-haskell/wasm/GameOfLifeWasm.hs` is the WASI/JSFFI entry (`foreign export javascript "runSteps" ...`) reading a tuple `([[Int]], Int)` and returning a JSON string; `core-haskell/wasm/build.sh` calls `wasm32-wasi-ghc` (needs `~/.ghc-wasm` in WSL2) and `node $LIBDIR/post-link.mjs` to produce `public/gameoflife.{wasm,js}`. Artifacts in `public/` are versioned so Pages builds don't require the toolchain.

## Migração / Git state

- The Haskell sources were moved from the repo root into `core-haskell/`: `git status` shows `GameOfLife.cabal`, `stack.yaml`, `src/Main.hs` as deleted (unstaged) and `core-haskell/` plus the entire frontend as untracked. HEAD still has Haskell at the root. The deletions are intentional — do not restore the root copies.
- Root `stack.yaml.lock`, `Setup.hs`, `.stack-work/`, and `dist-newstyle/` are leftovers from when Haskell lived at the root; `stack.yaml.lock` corresponds to the deleted root `stack.yaml`.
- `.gitignore` now covers `.stack-work/`, `dist-newstyle/` and `core-haskell/.stack-work/`, but old build artifacts are already tracked — don't commit new build outputs.
- `core-haskell/LICENSE` is a copy of the root `LICENSE` (cabal requires a license file next to the cabal file).

## Conventions

- Code comments and UI strings are in **Portuguese (pt-BR)**; match the existing language.
- TS config: project references (`tsconfig.app.json` + `tsconfig.node.json`) driven by `tsc -b`; no path aliases; `verbatimModuleSyntax` and `allowImportingTsExtensions` are on (imports may carry extensions), `noUnusedLocals`/`noUnusedParameters` are errors, `erasableSyntaxOnly` forbids enums/namespaces/parameter properties.
- ESLint is flat config (`eslint.config.js`), non-type-aware (the type-aware setup in `README.md` template docs is not enabled). `no-explicit-any` is an error — use `unknown` + narrowing.
- No Tailwind: everything is hand-written CSS in `src/index.css` (class names like `.app`, `.board`, `.btn`, `.rules`, `.speed`). `src/App.css` was removed. Don't add Tailwind classes expecting them to work.
- `README.md` is the project's own pt-BR README (it was restored from git HEAD, not the Vite template): it documents the university assignment (`Linguagem de Programação`, UFF - Niterói, 2021-2022), the console app, the game rules, and how to run both halves. Keep it in sync when changing behaviour.
- The game rules (Morta/Viva/Zumbi, including the zombie infection priority from `aliveDeadOrZombie`) are documented both in `README.md` and in the collapsible panel in `src/App.tsx`; update both together.
- Playback speed lives in `src/App.tsx` as `intervalMs` (slider, `MIN_INTERVAL_MS` 100 – `MAX_INTERVAL_MS` 2000, default `DEFAULT_INTERVAL_MS` 500); the play `useEffect` depends on it so changes apply while running.
