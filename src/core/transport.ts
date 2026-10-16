import { wasmTransport } from './wasmCore';

export type CellState = 0 | 1 | 2;

export interface GenerationResult {
  cells: CellState[][];
  iterationsUntilStable: number;
  stabilized: boolean;
}

export interface CoreTransport {
  health(): Promise<boolean>;
  run(cells: CellState[][], steps: number): Promise<GenerationResult>;
}

export type CoreKind = 'wasm';

export const coreKind: CoreKind = 'wasm';
export const coreTransport: CoreTransport = wasmTransport;

export function describeCore(): string {
  return 'Haskell em WASM (no navegador)';
}

