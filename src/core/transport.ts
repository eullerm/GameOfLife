// Camada de acesso ao núcleo do jogo.
//
// Duas implementações da mesma interface CoreTransport:
//   - wasmTransport (padrão): roda o núcleo Haskell compilado para WASM no
//     próprio navegador — funciona no GitHub Pages sem backend;
//   - httpTransport: fala com a API Haskell (core-haskell/server) por HTTP,
//     útil para rodar o backend separado (npm run api).
// A escolha vem de VITE_CORE (wasm | http). No HTTP, o endereço vem de
// VITE_API_URL (ex.: https://meu-backend.exemplo.com) ou do proxy /api.
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

export type CoreKind = 'wasm' | 'http';

const apiBase = ((import.meta.env.VITE_API_URL as string | undefined) ?? '').replace(/\/+$/, '');

function apiUrl(path: string): string {
  return `${apiBase}${path}`;
}

async function errorMessage(response: Response): Promise<string> {
  try {
    const body = (await response.json()) as { error?: unknown };
    if (typeof body.error === 'string') return body.error;
  } catch {
    /* resposta sem JSON */
  }
  return `HTTP ${response.status}`;
}

export const httpTransport: CoreTransport = {
  async health() {
    try {
      const response = await fetch(apiUrl('/api/health'));
      return response.ok;
    } catch {
      return false;
    }
  },

  async run(cells, steps) {
    const response = await fetch(apiUrl('/api/generation'), {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ cells, steps }),
    });
    if (!response.ok) {
      throw new Error(await errorMessage(response));
    }
    return (await response.json()) as GenerationResult;
  },
};

const requestedKind = ((import.meta.env.VITE_CORE as string | undefined) ?? '').toLowerCase();
export const coreKind: CoreKind = requestedKind === 'http' ? 'http' : 'wasm';

export const coreTransport: CoreTransport = coreKind === 'http' ? httpTransport : wasmTransport;

export function describeCore(): string {
  if (coreKind === 'wasm') return 'Haskell em WASM (no navegador)';
  return `Haskell via HTTP${apiBase ? ` (${apiBase})` : ' (/api)'}`;
}
