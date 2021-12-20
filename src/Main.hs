module Main where
import Data.List
import Data.Typeable

width, height :: Int
width = 80 -- tam da linha
height = 24 -- tam da coluna

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

type Pos = (Int,Int) -- coluna, linha
type Row = [(Pos, String)]
type Matrix = [Row]

-- Cria a matriz
initMatrix :: Int -> Int -> IO Matrix
initMatrix 1 width = 
  do 
    line <- getLine
    let lineSplit = words line -- Splita a linha de input do usuario
    let exactSize = getExactSize lineSplit 1 width
    return [exactSize]
initMatrix height width =
  do
    line <- getLine
    let lineSplit = words line -- Splita a linha de input do usuario
    let exactSize = getExactSize lineSplit height width
    nextLine <- initMatrix (height-1) width
    return (exactSize:nextLine)

-- Calcula o tamanho exato que cada vetor da matriz precisa ter
getExactSize :: [String] -> Int -> Int -> Row
getExactSize lineSplit row columns= 
  do
    let vetRow = take columns (repeat row)
    let vetColumn = [1..columns]
    let pos = zip vetRow vetColumn 
    let exactSize = take columns lineSplit
    let incrementSize = lineSplit ++ take (columns - (length lineSplit)) (repeat "D")
    if(length lineSplit == columns)
      then (zip pos exactSize)
    else if(length lineSplit < columns)
      then (zip pos incrementSize)
    else
      zip pos exactSize


printMatrix :: Matrix -> IO ()
printMatrix (vet:[]) = 
  do 
    printValue vet
printMatrix (vet:rest) =
  do 
    printValue vet
    printMatrix rest

printValue :: Row -> IO()
printValue [] = do putStrLn("|")
printValue (((r,c), value):rest) =
  do
    putStr ("|")
    putStr(show value)
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
