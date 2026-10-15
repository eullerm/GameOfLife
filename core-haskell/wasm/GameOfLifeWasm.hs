-- Entrada do núcleo Haskell compilado para WebAssembly.
--
-- Este arquivo NÃO substitui o programa de console (src/Main.hs) nem a API
-- HTTP (server/Main.hs): ele só reaproveita a MESMA lógica pura de
-- src/GameOfLifeApi.hs e a expõe para o navegador via JavaScript FFI (JSFFI)
-- do backend wasm do GHC.
--
-- Contrato (espelha POST /api/generation):
--   entrada (JSString): JSON de [cells, steps]  (ex.: "[[[1,0],[0,1]],5]")
--   saída   (JSString): JSON {"cells":[[...]],"iterationsUntilStable":n,"stabilized":bool}
--
-- Compilado por core-haskell/wasm/build.sh para ../public/gameoflife.wasm.
module GameOfLifeWasm (runSteps) where

import Data.List (intercalate)
import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString)
import GameOfLifeApi (step)

foreign export javascript "runSteps" runSteps :: JSString -> JSString

-- A saída é montada à mão para o módulo continuar dependendo apenas de base
-- (sem aeson/json).
toJsonCells :: [[Int]] -> String
toJsonCells rows = "[" ++ intercalate "," (map rowToJson rows) ++ "]"
  where
    rowToJson row = "[" ++ intercalate "," (map show row) ++ "]"

runSteps :: JSString -> JSString
runSteps input =
  case reads (fromJSString input) :: [(([[Int]], Int), String)] of
    (((cells, steps), _) : _) ->
      let width = case cells of
            (first : _) -> length first
            [] -> 0
          (result, iterations, stabilized) = step width cells steps
          json =
            "{\"cells\":"
              ++ toJsonCells result
              ++ ",\"iterationsUntilStable\":"
              ++ show iterations
              ++ ",\"stabilized\":"
              ++ (if stabilized then "true" else "false")
              ++ "}"
      in toJSString json
    [] -> toJSString "{\"error\":\"entrada invalida\"}"
