module Main where
import Data.List
import Data.Typeable

width, height :: Int
width = 80 -- tam da linha
height = 24 -- tam da coluna

type Pos = (Int,Int) -- coluna, linha
type Cells = [Pos] -- coordenadas das células

data Cell = L | D | Z deriving Show -- Define como uma célula pode ser

rightList (x:y:rest) = (x,y) : rightList (y:rest)
rightList _ = []

right m = m >>= rightList

swap (x,y) = (y,x)
co direction = map swap . direction
left = co right

down = right . transpose
up   = co down

downRight (xs:ys:rest) = zip xs (drop 1 ys) ++ downRight (ys:rest)
downRight _            = []
upLeft = co downRight

upRight  = downRight . reverse
downLeft = co upRight

allDirections = [right, left, up, down,
                 downRight, upLeft, upRight, downLeft]
neighbors m = allDirections >>= ($m) 

-- Retorna a posição de um elemento x numa lista
positions  :: Eq a => a -> [a] -> [Int]
positions x = map fst . filter ((x ==) . snd) . zip [0..]

-------------------------------------------------------------

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

printMatrix :: [[String]] -> IO ()
printMatrix (x:[]) = do 
    printValue x
printMatrix (x:rest) =
  do 
    printValue x
    printMatrix rest

printValue :: [String] -> IO()
printValue [] = do putStrLn("|")
printValue (x:rest) =
  do
    putStr ("|")
    putStr(show x)
    printValue rest

printDash :: Int -> [Char] -> IO()
printDash 0 char = do putStrLn(char)
printDash n char =
  do
    putStr(char)
    printDash (n-1) char

main :: IO ()
main = do

  putStrLn ("Tamanho da grid: ")
  gridSize <- getLine
  let gridSizeAsNumber = read gridSize :: Int

  putStrLn ("Celulas da grid: ")
  matrix <- initMatrix gridSizeAsNumber gridSizeAsNumber

  putStrLn ("Número de iterações: ")
  iteration <- getLine
  let iterationAsNumber = read iteration :: Int

  putStrLn ("Tamanho da grade: " ++ gridSize)

  putStrLn ("Matriz: \n" ++ show matrix)
  putStrLn ("\nTipo da matriz: " ++ show (typeOf matrix))
  -- putStrLn ("Matriz com sort: \n" ++ show (sort (neighbors matrix)))
  putStrLn ("\nPrint criado para matriz:")
  printDash (gridSizeAsNumber*4) "-"
  printMatrix matrix
  printDash (gridSizeAsNumber*4) "-"
  
  putStrLn ("Iteração: " ++ iteration)

  let s  = positions 'L' "foobaraboof"

  putStrLn (show s) 
