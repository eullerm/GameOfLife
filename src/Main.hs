module Main where

width, height :: Int
width = 80 -- tam da linha
height = 24 -- tam da coluna

type Pos = (Int,Int) -- coluna, linha
type Cells = [Pos] -- coordenadas das células

data Cell = L | D | Z deriving Show -- Define como uma célula pode ser


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
    let exactSize = getExactSize lineSplit width
    return [exactSize]
initMatrix height width =
  do
    line <- getLine
    let lineSplit = words line -- Splita a linha de input do usuario
    let exactSize = getExactSize lineSplit width
    nextLine <- initMatrix (height-1) width
    return (exactSize:nextLine)

-- Calcula o tamanho exato que cada vetor da matriz precisa ter
getExactSize :: [String] -> Int -> [String]
getExactSize lineSplit width = do
  if(length lineSplit == width)
    then lineSplit
  else if(length lineSplit < width)
    then lineSplit ++ take (width - (length lineSplit)) (repeat "D")
  else 
    take width lineSplit

printMatrix :: Int -> Int -> [[String]] -> String
printMatrix 1 width (x:[]) = 
  do
    show x
    --show ['2']
    --printVector width x
printMatrix height width (x:xs) = 
  do
    show x
    --show ['1']
    --printVector width x
    printMatrix (height-1) width xs

printVector :: Int -> [String] -> String
printVector 0 [] = "|"
printVector width (x:xs) =
  do
    show x
    printVector (width-1) xs 


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
  
  putStrLn ("Matriz: \n" ++ show matrix)

  putStrLn ("Iteração: " ++ iteration)
