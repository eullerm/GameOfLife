{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.List
import Data.List.Split
import Data.Typeable
import GHC.Arr
import System.IO
import Control.Monad
import Data.Char

type Row = [String]
type Matrix = [Row]

-- Transforma os caracteres em upperCase
uppercase :: String -> String
uppercase = map toUpper

-- Cria a matriz
initMatrix :: [Int] -> Int -> IO Matrix
initMatrix [1, width] row =
  do
    line <- getLine
    let lineSplit = words (uppercase line) -- Splita a linha de input do usuario
    let exactSize = getExactSize lineSplit row width
    return [exactSize]
initMatrix [height, width] row =
  do
    line <- getLine
    let lineSplit =  words (uppercase line)  -- Splita a linha de input do usuario
    let exactSize = getExactSize lineSplit row width
    nextLine <- initMatrix [height-1, width] (row + 1)
    return (exactSize:nextLine)

-- Calcula o tamanho exato que cada vetor da matriz precisa ter
getExactSize :: [String] -> Int -> Int -> Row
getExactSize lineSplit row columns=
  do
    let exactSize = take columns lineSplit
    let incrementSize = lineSplit ++ replicate (columns - length lineSplit) "M"
    if length lineSplit == columns
      then exactSize
    else if length lineSplit < columns
      then incrementSize
    else
      exactSize

-- Imprime a matriz
printMatrix :: Matrix -> IO ()
printMatrix [] =
  do
    return ()
printMatrix [vet] =
  do
    printValue vet
printMatrix (vet:rest) =
  do
    printValue vet
    printMatrix rest

-- Imprime a linha da matriz
printValue :: Row -> IO()
printValue [] = putStrLn "|"
printValue (value:rest) =
  do
    putStr "| "
    putStr value
    putStr " "
    printValue rest

printDash :: Int -> String -> IO()
printDash 0 char = putStrLn char
printDash n char =
  do
    putStr char
    printDash (n-1) char

-- Converte string para inteiros
stringToInts :: [String] -> [Int]
stringToInts = map read

-- Converte inteiro para strings
intsToString :: [Int] -> String
intsToString = map chr

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
getindexes i j = [[i-1,j+1], [i,j+1], [i+1,j+1],[i-1,j], [i+1,j],[i-1,j-1],[i, j-1], [i+1,j-1]]

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



main :: IO ()
main = do

  putStrLn "Tamanho da grid (Ex: 3x2): "
  gridSize <- getLine
  let griSizeUpperCase = uppercase gridSize
  let gridSizeAsNumber = stringToInts (splitOn "X" griSizeUpperCase)

  putStrLn "Celulas da grid separadas por espaços (V = Viva, M = Morta, Z = Zumbi): "
  matrix <- initMatrix gridSizeAsNumber 0

  putStrLn "Número de iterações máxima: "
  iteration <- getLine
  let iterationAsNumber = read iteration :: Int

  putStrLn "\nMatriz inicial:"
  printDash (4 * gridSizeAsNumber!!1 ) "-"
  printMatrix matrix
  printDash (4 * gridSizeAsNumber!!1 ) "-"

  let vetOfStatus = mergeAll matrix

  let game = gameOfLife vetOfStatus gridSizeAsNumber iterationAsNumber iterationAsNumber

  let iterationUntilNormalize = fst game
  let vetResult = snd game

  putStrLn ("\nNúmero de iterações até normalizar a matriz: " ++ show iterationUntilNormalize)

  let matrixResult = chunksOf (gridSizeAsNumber!!1) vetResult

  putStrLn "\nMatriz resultante:"
  printDash (4 * gridSizeAsNumber!!1 ) "-"
  printMatrix matrixResult
  printDash (4 * gridSizeAsNumber!!1 ) "-"

  