module GameOfLifeApi (step) where

import GameOfLife (gameOfLife)

-- Converte o estado numerico do frontend (0: Morta, 1: Viva, 2: Zumbi)
-- para o formato do console (M, V, Z)
fromInt :: Int -> Char
fromInt 0 = 'M'
fromInt 1 = 'V'
fromInt 2 = 'Z'
fromInt _ = 'M' -- inalcançavel: o servidor valida 0 <= estado <= 2

toInt :: Char -> Int
toInt 'M' = 0
toInt 'V' = 1
toInt 'Z' = 2
toInt _ = 0 -- inalcançavel: a lógica só produz M, V ou Z

-- Equivalente a chunksOf do pacote split, para o módulo depender apenas de base
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Passo puro compartilhado pela API HTTP (agora) e pela futura versão JS/WASM:
-- executa no máximo 'steps' gerações e devolve o estado final,
-- quantas gerações foram aplicadas até estabilizar e se estabilizou
-- (estabilizou = aplicou menos gerações do que o máximo pedido).
step :: Int -> [[Int]] -> Int -> ([[Int]], Int, Bool)
step width cells steps =
  let height = length cells
      flat = map (\c -> [fromInt c]) (concat cells)
      (iterations, result) = gameOfLife flat [height, width] steps steps
      rows = chunk width result
      back = map (map (toInt . head)) rows
  in (back, iterations, iterations < steps)
