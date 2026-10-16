import { useCallback, useEffect, useRef, useState } from 'react';
import Board from './components/Board';
import { coreTransport, describeCore } from './core/transport';
import type { CellState } from './core/transport';

const DEFAULT_ROWS = 20;
const DEFAULT_COLUMNS = 30;
const MIN_SIZE = 1;
const MAX_SIZE = 60;
const DEFAULT_INTERVAL_MS = 500;
const MIN_INTERVAL_MS = 100;
const MAX_INTERVAL_MS = 2000;
const INTERVAL_STEP_MS = 100;
const STABILIZE_STEPS = 500;

// 0: Morta, 1: Viva, 2: Zumbi
function emptyGrid(rows: number, columns: number): CellState[][] {
  return Array.from({ length: rows }, () => Array<CellState>(columns).fill(0));
}

function resizeGrid(cells: CellState[][], rows: number, columns: number): CellState[][] {
  return Array.from({ length: rows }, (_, row) =>
    Array.from({ length: columns }, (_, column) => cells[row]?.[column] ?? 0),
  );
}

function App() {
  const [cells, setCells] = useState<CellState[][]>(() => emptyGrid(DEFAULT_ROWS, DEFAULT_COLUMNS));
  const [rowsInput, setRowsInput] = useState(String(DEFAULT_ROWS));
  const [columnsInput, setColumnsInput] = useState(String(DEFAULT_COLUMNS));
  const [isRunning, setIsRunning] = useState(false);
  const [intervalMs, setIntervalMs] = useState(DEFAULT_INTERVAL_MS);
  const [generation, setGeneration] = useState(0);
  const [message, setMessage] = useState('');
  const [coreReady, setCoreReady] = useState<boolean | null>(null);

  // Mantém a última matriz à mão para o loop de reprodução não depender do estado do React
  const cellsRef = useRef(cells);
  const busyRef = useRef(false);

  useEffect(() => {
    cellsRef.current = cells;
  }, [cells]);

  useEffect(() => {
    let active = true;
    void coreTransport.health().then((online) => {
      if (active) setCoreReady(online);
    });
    return () => {
      active = false;
    };
  }, []);

  const runSteps = useCallback(async (steps: number) => {
    if (busyRef.current) return;
    busyRef.current = true;
    try {
      const result = await coreTransport.run(cellsRef.current, steps);
      setCells(result.cells);
      setGeneration((current) => current + (result.stabilized ? result.iterationsUntilStable : steps));
      if (result.stabilized) {
        setMessage(`Estabilizou em ${result.iterationsUntilStable} iteração(ões).`);
        setIsRunning(false);
      } else {
        setMessage('');
      }
    } catch (error) {
      const detail = error instanceof Error ? error.message : String(error);
      setMessage(`Erro ao falar com o motor Haskell: ${detail}`);
      setIsRunning(false);
    } finally {
      busyRef.current = false;
    }
  }, []);

  useEffect(() => {
    if (!isRunning) return;
    const id = window.setInterval(() => {
      void runSteps(1);
    }, intervalMs);
    return () => window.clearInterval(id);
  }, [isRunning, intervalMs, runSteps]);

  const handleToggle = useCallback((row: number, column: number) => {
    setCells((current) =>
      current.map((line, r) =>
        r !== row
          ? line
          : line.map((state, c) => (c !== column ? state : (((state + 1) % 3) as CellState))),
      ),
    );
  }, []);

  const handleClear = () => {
    setCells(emptyGrid(cells.length, cells[0]?.length ?? 0));
    setGeneration(0);
    setMessage('');
    setIsRunning(false);
  };

  const handleResize = () => {
    const rows = Number(rowsInput);
    const columns = Number(columnsInput);
    const valid =
      Number.isInteger(rows) &&
      Number.isInteger(columns) &&
      rows >= MIN_SIZE &&
      rows <= MAX_SIZE &&
      columns >= MIN_SIZE &&
      columns <= MAX_SIZE;
    if (!valid) {
      setMessage(`Dimensões inválidas: use inteiros entre ${MIN_SIZE} e ${MAX_SIZE}.`);
      return;
    }
    setCells((current) => resizeGrid(current, rows, columns));
    setGeneration(0);
    setMessage('');
    setIsRunning(false);
  };

  const coreLabel = coreReady === null ? 'carregando...' : coreReady ? 'pronto' : 'indisponível';
  const coreClass =
    coreReady === null ? 'app__api app__api--checking' : coreReady ? 'app__api app__api--online' : 'app__api app__api--offline';

  return (
    <div className="app">
      <header className="app__header">
        <h1>Game of Life</h1>
        <p className="app__subtitle">
          <a href="https://github.com/eullerm/GameOfLife" target="_blank" rel="noopener noreferrer">
            Ver código no GitHub
          </a>
        </p>
        <span className={coreClass}>
          {describeCore()}: {coreLabel}
        </span>
      </header>

      <Board cells={cells} onToggle={handleToggle} />

      <div className="controls">
        <button
          type="button"
          className="btn btn--primary"
          onClick={() => setIsRunning((running) => !running)}
          disabled={coreReady === false}
        >
          {isRunning ? 'Pausar' : 'Iniciar'}
        </button>
        <button type="button" className="btn" onClick={() => void runSteps(1)} disabled={coreReady === false}>
          Próxima geração
        </button>
        <button
          type="button"
          className="btn"
          onClick={() => void runSteps(STABILIZE_STEPS)}
          disabled={coreReady === false}
        >
          Rodar até estabilizar
        </button>
        <button type="button" className="btn" onClick={handleClear}>
          Limpar
        </button>
      </div>

      <div className="speed">
        <label htmlFor="velocidade">Velocidade</label>
        <span className="speed__hint">mais rápido</span>
        <input
          id="velocidade"
          type="range"
          min={MIN_INTERVAL_MS}
          max={MAX_INTERVAL_MS}
          step={INTERVAL_STEP_MS}
          value={intervalMs}
          onChange={(event) => setIntervalMs(Number(event.target.value))}
        />
        <span className="speed__hint">mais devagar</span>
        <span className="speed__value">{intervalMs} ms por geração</span>
      </div>

      <div className="resize">
        <label>
          Linhas
          <input value={rowsInput} onChange={(event) => setRowsInput(event.target.value)} inputMode="numeric" />
        </label>
        <label>
          Colunas
          <input value={columnsInput} onChange={(event) => setColumnsInput(event.target.value)} inputMode="numeric" />
        </label>
        <button type="button" className="btn" onClick={handleResize}>
          Aplicar
        </button>
      </div>

      <div className="status">
        <span>Gerações simuladas: {generation}</span>
        {message && <span className="status__message">{message}</span>}
      </div>

      <div className="legend">
        <span><i className="swatch swatch--morta" /> Morta</span>
        <span><i className="swatch swatch--viva" /> Viva</span>
        <span><i className="swatch swatch--zumbi" /> Zumbi</span>
        <span className="legend__hint">Clique numa célula para alternar Morta -&gt; Viva -&gt; Zumbi</span>
      </div>

      <details className="rules">
        <summary>Regras do jogo</summary>
        <div className="rules__body">
          <p>
            Cada célula tem oito vizinhos (incluindo as diagonais) e as bordas não dão a volta no tabuleiro. Em cada
            geração contam-se os vizinhos vivos (<code>a</code>) e os vizinhos zumbis (<code>z</code>):
          </p>
          <ul>
            <li>
              <strong>Morta</strong> — nasce <strong>Viva</strong> quando tem exatamente 3 vizinhos vivos; caso
              contrário continua morta (zumbi não afeta célula morta).
            </li>
            <li>
              <strong>Viva</strong> — com pelo menos 1 vizinho <strong>zumbi</strong> vira <strong>Zumbi</strong> (esta
              regra é aplicada antes das mortes); senão morre com menos de 2 ou com mais de 3 vizinhos vivos; senão
              continua viva.
            </li>
            <li>
              <strong>Zumbi</strong> — sem nenhum vizinho vivo volta a ser <strong>Morta</strong>; caso contrário
              continua zumbi (não morre por falta nem por excesso de vizinhos e não transforma células mortas).
            </li>
          </ul>
          <p>
            A infecção zumbi tem prioridade: uma célula viva com um único zumbi ao lado vira Zumbi independentemente da
            quantidade de vizinhos vivos, mas uma célula morta com 3 vizinhos vivos ainda nasce, mesmo que também toque
            um zumbi.
          </p>
        </div>
      </details>

      <details className="rules">
        <summary>Sobre o projeto</summary>
        <div className="rules__body">
          <p>
            O motor é o programa Haskell do trabalho da matéria de Linguagem de Programação (UFF - Niterói, 2021-2022), de
            Euller Macena e Malkai Oliveira. O frontend roda esse mesmo núcleo inteiramente no navegador, compilado para
            WebAssembly — sem servidor.
          </p>
          <p>
            Frontend desenvolvido por{' '}
            <a href="https://www.linkedin.com/in/euller-macena/" target="_blank" rel="noopener noreferrer">
              Euller Macena
            </a>
            .
          </p>
        </div>
      </details>
    </div>
  );
}

export default App;
