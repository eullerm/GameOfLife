import { useEffect, useRef } from 'react';
import type { MouseEvent } from 'react';
import type { CellState } from '../core/transport';

const CELL_SIZE = 20;

// 0: Morta, 1: Viva, 2: Zumbi
const CELL_COLORS: Record<CellState, string> = {
  0: '#1e293b',
  1: '#22c55e',
  2: '#ef4444',
};

interface BoardProps {
  cells: CellState[][];
  onToggle: (row: number, column: number) => void;
}

// Tabuleiro em canvas: desenha a matriz e avisa o App quando uma célula é clicada
function Board({ cells, onToggle }: BoardProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const rows = cells.length;
  const columns = cells[0]?.length ?? 0;

  useEffect(() => {
    const canvas = canvasRef.current;
    const context = canvas?.getContext('2d');
    if (!canvas || !context) return;

    context.fillStyle = '#0f172a';
    context.fillRect(0, 0, canvas.width, canvas.height);

    for (let row = 0; row < rows; row++) {
      for (let column = 0; column < columns; column++) {
        context.fillStyle = CELL_COLORS[cells[row][column]];
        context.fillRect(column * CELL_SIZE, row * CELL_SIZE, CELL_SIZE, CELL_SIZE);
      }
    }

    context.strokeStyle = 'rgba(148, 163, 184, 0.3)';
    context.lineWidth = 1;
    context.beginPath();
    for (let row = 0; row <= rows; row++) {
      context.moveTo(0, row * CELL_SIZE);
      context.lineTo(columns * CELL_SIZE, row * CELL_SIZE);
    }
    for (let column = 0; column <= columns; column++) {
      context.moveTo(column * CELL_SIZE, 0);
      context.lineTo(column * CELL_SIZE, rows * CELL_SIZE);
    }
    context.stroke();
  }, [cells, rows, columns]);

  const handleClick = (event: MouseEvent<HTMLCanvasElement>) => {
    const bounds = event.currentTarget.getBoundingClientRect();
    const column = Math.floor((event.clientX - bounds.left) / CELL_SIZE);
    const row = Math.floor((event.clientY - bounds.top) / CELL_SIZE);
    if (row >= 0 && row < rows && column >= 0 && column < columns) {
      onToggle(row, column);
    }
  };

  return (
    <canvas
      ref={canvasRef}
      width={columns * CELL_SIZE}
      height={rows * CELL_SIZE}
      className="board"
      onClick={handleClick}
    />
  );
}

export default Board;
