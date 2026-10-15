{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GameOfLife where

first :: (a, b, c) -> a
first (a,_,_) = a
second :: (a, b, c) -> b
second (_,b,_) = b
third :: (a, b, c) -> c
third (_,_,c) = c

-- Conta os vizinhos e verifica se são vivos, mortos ou zumbis
countNeighbors ::[[Char]] -> Int -> Int -> [[Int]] -> (Int, Int, Int) -> (Int, Int, Int)
countNeighbors matrix row column [] (a,b,c) = (a,b,c)
countNeighbors matrix row column indexes (a,b,c)
    | i < 0 || j < 0 = countNeighbors matrix row column body (a, b, c)
    | i >= row || j >= column = countNeighbors matrix row column body (a, b, c)
    | matrix!!index == "M" = countNeighbors matrix row column body (a, b + 1, c)
    | matrix!!index == "V" = countNeighbors matrix row column body (a + 1, b, c)
    | matrix!!index == "Z" = countNeighbors matrix row column body (a, b, c + 1)
    where body = tail indexes
          i = head indexes !! 0
          j = head indexes !! 1
          index = i * column + j

-- Pega todos os indices adjacentes 
getindexes :: Int -> Int -> [[Int]]
getindexes i j = [[i-1,j+1], [i,j+1], [i+1,j+1],[i-1,j], [i+1,j],[i-1,j-1], [i, j-1], [i+1,j-1]]

-- Função auxiliar para buscar os índices e contar os estados
adz :: [[Char]] -> Int -> Int -> Int -> (Int, Int, Int)
adz matrix row column i = countNeighbors matrix row column (getindexes (div i column) (mod i column)) (0,0,0)

-- Classifica uma posição como viva, morta ou zumbi
aliveDeadOrZombie :: [[Char]] -> Int -> Int -> Int -> [Char]
aliveDeadOrZombie n row column i
   | n!!i == "M" && a == 3 = "V"
   | n!!i == "V" && z >= 1 = "Z"
   | n!!i == "V" && a < 2  = "M"
   | n!!i == "V" && a > 3  = "M"
   | n!!i == "Z" && a == 0 = "M"
   | otherwise = n!!i
   where a = first (adz n row column i)
         d = second (adz n row column i)
         z = third (adz n row column i)

-- Percorre todos as posições da tabela passada, classificando cada posição e adicionando na lista 'b'
travel :: [[Char]] -> Int -> Int -> [[Char]] -> Int -> [[Char]]
travel n row column newVet i
   | i >= row * column = newVet
   | otherwise = travel n row column c (i + 1)
   where c = (aliveDeadOrZombie n row column i):newVet

-- Quantas vezes percorrer toda a tabela e funcao checa se: tabela atual == tabela passada
gameOfLife :: [[Char]] -> [Int] -> Int -> Int -> (Int, [[Char]])
gameOfLife matrix [row, column] iteration total
  | iteration > 0 && newMatrix /= matrix = gameOfLife newMatrix [row, column] (iteration - 1) total -- Se não: continuar execução
  | iteration > 0 && newMatrix == matrix = ((total - iteration), matrix) -- Se sim: parar e retornar qtd de rodadas ate o momento e tabela atual
  | otherwise = ((total - iteration), matrix) -- Se chegar ao fim do numero total de iteracoes sem estabilizar: retorna estado final da tabela
  where newMatrix = reverse (travel matrix row column [] 0)

-- Faz o merge de todos os vetores da matriz
mergeAll :: Ord a => [[a]] -> [a]
mergeAll [] = []
mergeAll x = head $ mergePairs x

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs [] = []
mergePairs (x:[]) = [x]
mergePairs (x:y:tail) = mergePairs ((mergeTwo x y):(mergePairs tail))

mergeTwo :: Ord a => [a] -> [a] -> [a]
mergeTwo x [] = x
mergeTwo [] x = x
mergeTwo x y = x++y
