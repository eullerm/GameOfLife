// Transporte que roda o núcleo Haskell no próprio navegador, sem backend.
//
// O .wasm é gerado por core-haskell/wasm/build.sh (WASI reactor + JavaScript
// FFI do backend wasm do GHC) e o glue js vem do post-link.mjs. A camada WASI
// no navegador é fornecida pelo @bjorn3/browser_wasi_shim.
import { ConsoleStdout, File, OpenFile, WASI } from '@bjorn3/browser_wasi_shim';
import type { CoreTransport, GenerationResult } from './transport';

type JsffiFactory = (exports: Record<string, unknown>) => Record<string, unknown>;
type RunSteps = (input: string) => Promise<string>;

const WASM_FILE = 'gameoflife.wasm';
const GLUE_FILE = 'gameoflife.js';

function assetUrl(file: string): string {
  const base = import.meta.env.BASE_URL || '/';
  return new URL(base + file, window.location.origin).href;
}

let corePromise: Promise<RunSteps> | null = null;

async function loadRunSteps(): Promise<RunSteps> {
  const glue = (await import(/* @vite-ignore */ assetUrl(GLUE_FILE))) as { default: JsffiFactory };

  // Knot-tying: os imports ghc_wasm_jsffi só podem ser montados depois de
  // conhecermos os exports do módulo, então usamos um objeto mutável.
  const wasmExports: Record<string, unknown> = {};

  const fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered(() => {}), // stdout
    ConsoleStdout.lineBuffered(() => {}), // stderr
  ];
  const wasi = new WASI([], [], fds);

  const imports = {
    ghc_wasm_jsffi: glue.default(wasmExports),
    wasi_snapshot_preview1: wasi.wasiImport,
  } as unknown as WebAssembly.Imports;

  const response = await fetch(assetUrl(WASM_FILE));
  const bytes = await response.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes, imports);

  Object.assign(wasmExports, instance.exports);
  wasi.initialize(instance as unknown as { exports: { memory: WebAssembly.Memory; _initialize?: () => unknown } });

  return instance.exports.runSteps as unknown as RunSteps;
}

export const wasmTransport: CoreTransport = {
  async health() {
    try {
      await (corePromise ??= loadRunSteps());
      return true;
    } catch {
      return false;
    }
  },

  async run(cells, steps) {
    corePromise ??= loadRunSteps();
    const runSteps = await corePromise;
    // O lado Haskell faz "read" de uma tupla ([[Int]], Int); a sintaxe de
    // lista do JSON coincide com a do Haskell, só trocamos [] por ().
    const json = await runSteps(`(${JSON.stringify(cells)},${steps})`);
    const parsed = JSON.parse(json) as GenerationResult & { error?: string };
    if (typeof parsed.error === 'string') {
      throw new Error(parsed.error);
    }
    return parsed;
  },
};
