{- cabal:
build-depends: base, random >= 1.2
-}
{-# LANGUAGE ScopedTypeVariables #-}
import System.Environment
import System.Random.Stateful
import Data.Int
import Data.Proxy
import System.IO
import Control.Monad
import Data.List

showAsSMLInt :: (Integral a, Show a) => a -> String
showAsSMLInt x | x < 0 = '~' : tail (show x)
               | otherwise = show x

run :: forall a. (Integral a, Show a, Random a, Bounded a) => String -> Proxy a -> IO ()
run name _ = do
  putStrLn $ "fun call f arg = print ((" ++ name ++ ".toString (f arg) handle Overflow => \"Overflow\" | Div => \"Div\") ^ \"\\n\")"
  putStrLn $ "fun callS f arg = print ((f arg handle Overflow => \"Overflow\" | Div => \"Div\") ^ \"\\n\")"
  putStrLn $ "fun bin (x, y) = ( print (" ++ name ++ ".toString x ^ \" + \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".+ (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" - \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".- (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" * \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".* (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" div \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".div (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" mod \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".mod (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" quot \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".quot (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" rem \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".rem (x, y)"
  putStrLn $ "                 );"
  putStrLn $ "fun un x = ( print (\"~ \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; call " ++ name ++ ".~ x"
  putStrLn $ "           ; print (\"abs \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; call " ++ name ++ ".abs x"
  putStrLn $ "           (* ; print (\"fmt BIN \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; callS (" ++ name ++ ".fmt StringCvt.BIN) x ... not implemented yet *)"
  putStrLn $ "           ; print (\"fmt OCT \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; callS (" ++ name ++ ".fmt StringCvt.OCT) x"
  putStrLn $ "           ; print (\"fmt DEC \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; callS (" ++ name ++ ".fmt StringCvt.DEC) x"
  putStrLn $ "           ; print (\"fmt HEX \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; callS (" ++ name ++ ".fmt StringCvt.HEX) x"
  putStrLn $ "           );"
  g <- newIOGenM (mkStdGen 42)
  randomCases1 <- replicateM 50 (randomM g :: IO a)
  let allCases1 = nub ([minBound,minBound+1,-77,-3,-2,-1,0,1,2,3,78,maxBound-1,maxBound] ++ randomCases1)
  putStrLn $ "List.app un\n[" ++ concat (intersperse "\n," [showAsSMLInt x | x <- allCases1]) ++ "\n];"
  let miniCases2 = [ (x, y)
                   | x <- [minBound,minBound+1,-77,-3,-2,-1,0,1,2,3,78,maxBound-1,maxBound]
                   , y <- [minBound,minBound+1,-35,-3,-2,-1,0,1,2,3,99,maxBound-1,maxBound]
                   ]
  randomCases2 <- replicateM 1000 $ do
    x <- randomM g :: IO a
    y <- randomM g :: IO a
    pure (x, y)
  let allCases2 = nub (miniCases2 ++ randomCases2)
  putStrLn $ "List.app bin\n[" ++ concat (intersperse "\n," ["(" ++ showAsSMLInt x ++ "," ++ showAsSMLInt y ++ ")" | (x, y) <- allCases2]) ++ "\n];"

main = do
  args <- getArgs
  case args of
    "Int8" : _ -> run "Int8" (Proxy :: Proxy Int8)
    "Int16" : _ -> run "Int16" (Proxy :: Proxy Int16)
    "Int32" : _ -> run "Int32" (Proxy :: Proxy Int32)
    "Int64" : _ -> run "Int64" (Proxy :: Proxy Int64)
    _ -> hPutStrLn stderr "unexpected argument"

-- run as:
-- cabal run GenIntTest.hs Int8 > int8.sml
-- cabal run GenIntTest.hs Int16 > int16.sml
-- cabal run GenIntTest.hs Int32 > int32.sml
-- cabal run GenIntTest.hs Int64 > int64.sml
