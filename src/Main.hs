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

-- repeatNtimes :: String -> IO [Char]
repeatNTimes 0 = return []
repeatNTimes n =
  do
  line <- getLine
  nextLine <- repeatNTimes (n-1)
  return (line:nextLine)
  

main :: IO ()
main = do

  putStrLn ("Tamanho da grid: ")
  gridSize <- getLine
  let gridSizeAsNumber = read gridSize :: Int

  putStrLn ("Celulas da grid: ")
  matrix <- getLine

  putStrLn ("Número de iterações: ")
  iteration <- getLine
  let getLineAsNumber = read gridSize :: Int

  putStrLn ("Tamanho da grade: " ++ gridSize)
  putStrLn ("Matriz: \n" ++ matrix)
  putStrLn ("Iteração: " ++ iteration)
