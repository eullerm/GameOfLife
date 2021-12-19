module Main where

width, height :: Int
width = 80 -- tam da linha
height = 24 -- tam da coluna

type Pos = (Int,Int) -- coluna, linha
type Cells = [Pos] -- coordenadas das células

data Cell = L Int | D Int | Z Int -- Define como uma célula pode ser


-- obter as 8 posições vizinhas
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y) , (x-1,y+1),
                          (x,y+1) , (x+1,y+1)]

-- garantir que uma posição está dentro do tabuleiro
wrap :: Pos -> Pos
wrap (x,y) = ((x-1) `mod` width + 1, (y-1) `mod` height + 1)

-- Cria a matriz
initMatrix :: Int -> Int -> IO [[String]]
initMatrix 1 width = 
  do 
    line <- getLine
    let lineSplit = words line -- Splita a linha de input do usuario
    let exactSize = lineSplit ++ take (width - (length lineSplit)) (repeat "D") -- Calcula o tamanho exato que a matriz precisa ter
    return [exactSize]
initMatrix height width =
  do
    line <- getLine
    let lineSplit = words line -- Splita a linha de input do usuario
    let exactSize = lineSplit ++ take (width - (length lineSplit)) (repeat "D")-- Calcula o tamanho exato que a matriz precisa ter
    nextLine <- initMatrix (height-1) width
    return (exactSize:nextLine)
  


{- printNTimes :: Int -> Int -> [[String]] -> IO String
printNTimes 1 width (vector:[]) = show vector
printNTimes height width (x:xs) =
  do
    show x
    printNTimes (height-1) width xs -}

main :: IO ()
main = do

  putStrLn ("Tamanho da grid: ")
  gridSize <- getLine
  let gridSizeAsNumber = read gridSize :: Int

  putStrLn ("Celulas da grid: ")
  matrix <- initMatrix gridSizeAsNumber gridSizeAsNumber

  putStrLn ("Número de iterações: ")
  iteration <- getLine
  let getLineAsNumber = read gridSize :: Int

  putStrLn ("Tamanho da grade: " ++ gridSize)
  
  putStrLn ("Matriz: \n" ++ show matrix )

  putStrLn ("Iteração: " ++ iteration)
