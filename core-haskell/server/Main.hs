{-# LANGUAGE OverloadedStrings #-}

-- API HTTP local que expõe o núcleo Game of Life para o frontend.
-- Build/run: stack build && stack exec api  (porta 8787; o Vite faz proxy de /api)
module Main where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Text.Lazy as TL
import GameOfLifeApi (step)
import Network.HTTP.Types.Status (status400)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

data GenReq = GenReq { reqCells :: [[Int]], reqSteps :: Int }

data GenRes = GenRes { resCells :: [[Int]], resIterations :: Int, resStabilized :: Bool }

instance FromJSON GenReq where
  parseJSON = withObject "GenReq" $ \o -> GenReq <$> o .: "cells" <*> o .: "steps"

instance ToJSON GenRes where
  toJSON (GenRes cells iterations stabilized) =
    object [ "cells" .= cells
           , "iterationsUntilStable" .= iterations
           , "stabilized" .= stabilized
           ]

-- A lógica do console é parcial (read/!!), então só chamamos com entradas válidas
validReq :: GenReq -> Bool
validReq (GenReq cells steps) =
  let height = length cells
      width = if null cells then 0 else length (head cells)
  in height >= 1 && height <= 200
     && width >= 1 && width <= 200
     && all ((== width) . length) cells
     && all (\row -> all (\c -> c >= 0 && c <= 2) row) cells
     && steps >= 0 && steps <= 10000

main :: IO ()
main = do
  portEnv <- lookupEnv "GOL_API_PORT"
  let port = maybe 8787 id (portEnv >>= readMaybe)
  scotty port $ do
    get "/api/health" $
      text "ok"

    post "/api/generation" $ do
      req <- jsonData :: ActionM GenReq
      if not (validReq req)
        then do
          status status400
          json (object ["error" .= TL.pack "requisicao invalida: grade vazia/nao retangular, celula fora de 0..2 ou steps fora de 0..10000"])
        else do
          let GenReq cells steps = req
              width = length (head cells)
              (cells', iterations, stabilized) = step width cells steps
          json (GenRes cells' iterations stabilized)
