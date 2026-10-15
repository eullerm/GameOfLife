// Camada de acesso ao núcleo do jogo.
// Hoje fala com o executável Haskell (core-haskell, `npm run api`) via HTTP local.
// Se um dia o Haskell virar WebAssembly/GHC-JS, basta criar outro CoreTransport
// e trocar `coreTransport` abaixo, sem mexer nos componentes.
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

async function errorMessage(response: Response): Promise<string> {
  const body = (await response.json().catch(() => null)) as unknown;
  if (body && typeof body === 'object' && 'error' in body) {
    const detail = (body as { error?: unknown }).error;
    if (typeof detail === 'string') return detail;
  }
  return `HTTP ${response.status}`;
}

export const httpTransport: CoreTransport = {
  async health() {
    try {
      const response = await fetch('/api/health');
      return response.ok;
    } catch {
      return false;
    }
  },

  async run(cells, steps) {
    const response = await fetch('/api/generation', {
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

export const coreTransport: CoreTransport = httpTransport;
