{- cabal:
build-depends: base, random >= 1.2
-}
{-# LANGUAGE ScopedTypeVariables #-}
import System.Environment
import System.Random.Stateful
import Data.Word
import Data.Proxy
import System.IO
import Control.Monad
import Data.List

showAsSMLWord :: (Integral a, Show a) => a -> String
showAsSMLWord x = "0w" ++ show x

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
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" andb \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".andb (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" orb \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".orb (x, y)"
  putStrLn $ "                 ; print (" ++ name ++ ".toString x ^ \" xorb \" ^ " ++ name ++ ".toString y ^ \" = \")"
  putStrLn $ "                 ; call " ++ name ++ ".xorb (x, y)"
  putStrLn $ "                 );"
  putStrLn $ "fun un x = ( print (\"~ \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; call " ++ name ++ ".~ x"
  putStrLn $ "           ; print (\"notb \" ^ " ++ name ++ ".toString x ^ \" = \")"
  putStrLn $ "           ; call " ++ name ++ ".notb x"
  forM_ [0,2,10,20,31,63,64,65] $ \a -> do
    putStrLn $ "           ; print (" ++ name ++ ".toString x ^ \" << 0w" ++ show a ++ " = \")"
    putStrLn $ "           ; call (fn x => " ++ name ++ ".<< (x, 0w" ++ show a ++ ")) x"
  forM_ [0,2,10,20,31,63,64,65] $ \a -> do
    putStrLn $ "           ; print (" ++ name ++ ".toString x ^ \" >> 0w" ++ show a ++ " = \")"
    putStrLn $ "           ; call (fn x => " ++ name ++ ".>> (x, 0w" ++ show a ++ ")) x"
  forM_ [0,2,10,20,31,63,64,65] $ \a -> do
    putStrLn $ "           ; print (" ++ name ++ ".toString x ^ \" ~>> 0w" ++ show a ++ " = \")"
    putStrLn $ "           ; call (fn x => " ++ name ++ ".~>> (x, 0w" ++ show a ++ ")) x"
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
  let allCases1 = nub ([0,1,2,3,78,maxBound-1,maxBound] ++ randomCases1)
  putStrLn $ "List.app un\n[" ++ concat (intersperse "\n," [showAsSMLWord x | x <- allCases1]) ++ "\n];"
  let miniCases2 = [ (x, y)
                   | x <- [0,1,2,3,78,maxBound-1,maxBound]
                   , y <- [0,1,2,3,99,maxBound-1,maxBound]
                   ]
  randomCases2 <- replicateM 1000 $ do
    x <- randomM g :: IO a
    y <- randomM g :: IO a
    pure (x, y)
  let allCases2 = nub (miniCases2 ++ randomCases2)
  putStrLn $ "List.app bin\n[" ++ concat (intersperse "\n," ["(" ++ showAsSMLWord x ++ "," ++ showAsSMLWord y ++ ")" | (x, y) <- allCases2]) ++ "\n];"

main = do
  args <- getArgs
  case args of
    "Word8" : _ -> run "Word8" (Proxy :: Proxy Word8)
    "Word16" : _ -> run "Word16" (Proxy :: Proxy Word16)
    "Word32" : _ -> run "Word32" (Proxy :: Proxy Word32)
    "Word64" : _ -> run "Word64" (Proxy :: Proxy Word64)
    _ -> hPutStrLn stderr "unexpected argument"

-- run as:
-- cabal run GenWordTest.hs Word8 > word8.sml
-- cabal run GenWordTest.hs Word16 > word16.sml
-- cabal run GenWordTest.hs Word32 > word32.sml
-- cabal run GenWordTest.hs Word64 > word64.sml
