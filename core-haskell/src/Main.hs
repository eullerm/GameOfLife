{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.List
import Data.List.Split
import Data.Typeable
import GHC.Arr
import System.IO
import Control.Monad
import Data.Char
import GameOfLife

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

  