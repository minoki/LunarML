{- cabal:
build-depends: base, random >= 1.2
-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Numeric
import System.Environment
import System.IO
import System.Random.Stateful

showAsSMLInt :: (Integral a, Show a) => a -> String
showAsSMLInt x | x < 0 = '~' : tail (show x)
               | otherwise = show x

showAsSMLInt' :: (Integer -> ShowS) -> Integer -> String
showAsSMLInt' s x | x < 0 = '~' : (s (-x)) ""
                  | otherwise = s x ""

showUpperHex :: Integer -> ShowS
showUpperHex n = (map toUpper (showHex n "") ++)

data Result a = Overflow | Div | Success a

checkOverflow :: Maybe Int -> Integer -> Result Integer
checkOverflow (Just w) z = if -2^(w-1) <= z && z <= 2^(w-1)-1 then
                             Success z
                           else
                             Overflow
checkOverflow Nothing z = Success z

add :: Maybe Int -> Integer -> Integer -> Result Integer
add w x y = checkOverflow w $ x + y

sub :: Maybe Int -> Integer -> Integer -> Result Integer
sub w x y = checkOverflow w $ x - y

mul :: Maybe Int -> Integer -> Integer -> Result Integer
mul w x y = checkOverflow w $ x * y

div' :: Maybe Int -> Integer -> Integer -> Result Integer
div' _ _ 0 = Div
div' w x y = checkOverflow w $ x `div` y

mod' :: Maybe Int -> Integer -> Integer -> Result Integer
mod' _ _ 0 = Div
mod' w x y = checkOverflow w $ x `mod` y

divMod' :: Maybe Int -> Integer -> Integer -> Result (Integer, Integer)
divMod' _ _ 0 = Div
divMod' w x y = let (q, r) = x `divMod` y
                in case (checkOverflow w q, checkOverflow w r) of
                     (Success _, Success _) -> Success (q, r)
                     _ -> Overflow

quot' :: Maybe Int -> Integer -> Integer -> Result Integer
quot' _ _ 0 = Div
quot' w x y = checkOverflow w $ x `quot` y

rem' :: Maybe Int -> Integer -> Integer -> Result Integer
rem' _ _ 0 = Div
rem' w x y = checkOverflow w $ x `rem` y

quotRem' :: Maybe Int -> Integer -> Integer -> Result (Integer, Integer)
quotRem' _ _ 0 = Div
quotRem' w x y = let (q, r) = x `quotRem` y
                 in case (checkOverflow w q, checkOverflow w r) of
                      (Success _, Success _) -> Success (q, r)
                      _ -> Overflow

showResult :: Result Integer -> String
showResult Overflow = "Overflow"
showResult Div = "Div"
showResult (Success x) = showAsSMLInt x

showResultSOME :: Result Integer -> String
showResultSOME Overflow = "Overflow"
showResultSOME Div = "Div"
showResultSOME (Success x) = "SOME " ++ showAsSMLInt x

showResultPair :: Result (Integer, Integer) -> String
showResultPair Overflow = "Overflow"
showResultPair Div = "Div"
showResultPair (Success (x, y)) = "(" ++ showAsSMLInt x ++ "," ++ showAsSMLInt y ++ ")"

run :: String -> Maybe Int -> (IOGenM StdGen -> IO Integer) -> (IOGenM StdGen -> IO Integer) -> Handle -> Handle -> IO ()
run name width rand randX code out = do
  hPutStrLn code $ "fun call f arg = print ((" ++ name ++ ".toString (f arg) handle Overflow => \"Overflow\" | Div => \"Div\") ^ \"\\n\")"
  hPutStrLn code $ "fun callS f arg = print ((f arg handle Overflow => \"Overflow\" | Div => \"Div\") ^ \"\\n\")"
  hPutStrLn code $ "fun callO f arg = print (((case f arg of SOME x => \"SOME \" ^ " ++ name ++ ".toString x | NONE => \"NONE\") handle Overflow => \"Overflow\" | Div => \"Div\") ^ \"\\n\")"
  hPutStrLn code $ "fun callP f arg = print (((case f arg of (x, y) => \"(\" ^ " ++ name ++ ".toString x ^ \",\" ^ " ++ name ++ ".toString y ^ \")\") handle Overflow => \"Overflow\" | Div => \"Div\") ^ \"\\n\")"
  hPutStrLn code $ "fun bin (x, y) = ( print (" ++ name ++ ".toString x ^ \" + \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".+ (x, y)"
  hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" - \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".- (x, y)"
  hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" * \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".* (x, y)"
  hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" div \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".div (x, y)"
  hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" mod \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".mod (x, y)"
  when (name == "IntInf") $ do
    hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" divMod \" ^ " ++ name ++ ".toString y ^ \" = \")"
    hPutStrLn code $ "                 ; callP " ++ name ++ ".divMod (x, y)"
  hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" quot \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".quot (x, y)"
  hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" rem \" ^ " ++ name ++ ".toString y ^ \" = \")"
  hPutStrLn code $ "                 ; call " ++ name ++ ".rem (x, y)"
  when (name == "IntInf") $ do
    hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" quotRem \" ^ " ++ name ++ ".toString y ^ \" = \")"
    hPutStrLn code $ "                 ; callP " ++ name ++ ".quotRem (x, y)"
    hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" orb \" ^ " ++ name ++ ".toString y ^ \" = \")"
    hPutStrLn code $ "                 ; call " ++ name ++ ".orb (x, y)"
    hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" xorb \" ^ " ++ name ++ ".toString y ^ \" = \")"
    hPutStrLn code $ "                 ; call " ++ name ++ ".xorb (x, y)"
    hPutStrLn code $ "                 ; print (" ++ name ++ ".toString x ^ \" andb \" ^ " ++ name ++ ".toString y ^ \" = \")"
    hPutStrLn code $ "                 ; call " ++ name ++ ".andb (x, y)"
  hPutStrLn code $ "                 );"
  hPutStrLn code $ "fun un x = ( print (\"~ \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; call " ++ name ++ ".~ x"
  hPutStrLn code $ "           ; print (\"abs \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; call " ++ name ++ ".abs x"
  when (name == "IntInf") $ do
    hPutStrLn code $ "           ; print (\"notb \" ^ " ++ name ++ ".toString x ^ \" = \")"
    hPutStrLn code $ "           ; call " ++ name ++ ".notb x"
  hPutStrLn code $ "           (* ; print (\"fmt BIN \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; callS (" ++ name ++ ".fmt StringCvt.BIN) x ... not implemented yet *)"
  hPutStrLn code $ "           ; print (\"fmt OCT \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; callS (" ++ name ++ ".fmt StringCvt.OCT) x"
  hPutStrLn code $ "           ; print (\"fmt DEC \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; callS (" ++ name ++ ".fmt StringCvt.DEC) x"
  hPutStrLn code $ "           ; print (\"fmt HEX \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; callS (" ++ name ++ ".fmt StringCvt.HEX) x"
  hPutStrLn code $ "           ; print (\"toLarge \" ^ " ++ name ++ ".toString x ^ \" = \")"
  hPutStrLn code $ "           ; callS (LargeInt.toString o " ++ name ++ ".toLarge) x"
  hPutStrLn code $ "           );"
  g <- newIOGenM (mkStdGen 42)
  randomCases1 <- replicateM 50 (rand g)
  let minB = case width of
               Just w -> -2^(w-1)
               Nothing -> -2^63
      maxB = case width of
               Just w -> 2^(w-1)-1
               Nothing -> 2^63-1
  let extraCase = if fromMaybe True ((>= 54) <$> width) then
                    [3002399751580331] -- (2^53+1) `div` 3
                  else
                    []
  let allCases1 = nub ([minB,minB+1,-77,-3,-2,-1,0,1,2,3,78,maxB-1,maxB] ++ randomCases1)
  hPutStrLn code $ "List.app un\n[" ++ concat (intersperse "\n," [showAsSMLInt x | x <- allCases1]) ++ "\n];"
  if name == "IntInf"
    then hPutStr out $ concat ["~ " ++ showAsSMLInt x ++ " = " ++ showResult (checkOverflow width (-x)) ++ "\n\
                               \abs " ++ showAsSMLInt x ++ " = " ++ showResult (checkOverflow width (abs x)) ++ "\n\
                               \notb " ++ showAsSMLInt x ++ " = " ++ showResult (checkOverflow width (complement x)) ++ "\n\
                               \fmt OCT " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt' showOct x ++ "\n\
                               \fmt DEC " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt x ++ "\n\
                               \fmt HEX " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt' showUpperHex x ++ "\n\
                               \toLarge " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt x ++ "\n" | x <- allCases1]
    else hPutStr out $ concat ["~ " ++ showAsSMLInt x ++ " = " ++ showResult (checkOverflow width (-x)) ++ "\n\
                               \abs " ++ showAsSMLInt x ++ " = " ++ showResult (checkOverflow width (abs x)) ++ "\n\
                               \fmt OCT " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt' showOct x ++ "\n\
                               \fmt DEC " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt x ++ "\n\
                               \fmt HEX " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt' showUpperHex x ++ "\n\
                               \toLarge " ++ showAsSMLInt x ++ " = " ++ showAsSMLInt x ++ "\n" | x <- allCases1]
  let miniCases2 = [ (x, y)
                   | x <- [minB,minB+1,-77,-3,-2,-1,0,1,2,3,78,maxB-1,maxB]
                   , y <- [minB,minB+1,-35,-3,-2,-1,0,1,2,3,99,maxB-1,maxB] ++ extraCase
                   ]
  randomCases2 <- replicateM 1000 $ liftA2 (,) (rand g) (rand g)
  let allCases2 = nub (miniCases2 ++ randomCases2)
  hPutStrLn code $ "List.app bin\n[" ++ concat (intersperse "\n," ["(" ++ showAsSMLInt x ++ "," ++ showAsSMLInt y ++ ")" | (x, y) <- allCases2]) ++ "\n];"
  if name == "IntInf"
    then hPutStr out $ concat [showAsSMLInt x ++ " + " ++ showAsSMLInt y ++ " = " ++ showResult (add width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " - " ++ showAsSMLInt y ++ " = " ++ showResult (sub width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " * " ++ showAsSMLInt y ++ " = " ++ showResult (mul width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " div " ++ showAsSMLInt y ++ " = " ++ showResult (div' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " mod " ++ showAsSMLInt y ++ " = " ++ showResult (mod' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " divMod " ++ showAsSMLInt y ++ " = " ++ showResultPair (divMod' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " quot " ++ showAsSMLInt y ++ " = " ++ showResult (quot' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " rem " ++ showAsSMLInt y ++ " = " ++ showResult (rem' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " quotRem " ++ showAsSMLInt y ++ " = " ++ showResultPair (quotRem' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " orb " ++ showAsSMLInt y ++ " = " ++ showAsSMLInt (x .|. y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " xorb " ++ showAsSMLInt y ++ " = " ++ showAsSMLInt (x `xor` y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " andb " ++ showAsSMLInt y ++ " = " ++ showAsSMLInt (x .&. y) ++ "\n"
                              | (x, y) <- allCases2]
    else hPutStr out $ concat [showAsSMLInt x ++ " + " ++ showAsSMLInt y ++ " = " ++ showResult (add width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " - " ++ showAsSMLInt y ++ " = " ++ showResult (sub width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " * " ++ showAsSMLInt y ++ " = " ++ showResult (mul width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " div " ++ showAsSMLInt y ++ " = " ++ showResult (div' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " mod " ++ showAsSMLInt y ++ " = " ++ showResult (mod' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " quot " ++ showAsSMLInt y ++ " = " ++ showResult (quot' width x y) ++ "\n\
                               \" ++ showAsSMLInt x ++ " rem " ++ showAsSMLInt y ++ " = " ++ showResult (rem' width x y) ++ "\n"
                              | (x, y) <- allCases2]
  bigCases <- replicateM 100 (randX g)
  let exCases = allCases1 ++ bigCases
  hPutStrLn code $ "List.app (fn x => (print (\"fromLarge \" ^ LargeInt.toString x ^ \" = \"); call " ++ name ++ ".fromLarge x))\n[" ++ concat (intersperse "\n," [showAsSMLInt x | x <- exCases]) ++ "\n];"
  hPutStr out $ unlines ["fromLarge " ++ showAsSMLInt x ++ " = " ++ showResult (checkOverflow width x) | x <- exCases]
  hPutStrLn code $ "List.app (fn s => (print (\"fromString \" ^ s ^ \" = \"); callO " ++ name ++ ".fromString s))\n[" ++ concat (intersperse "\n," ['"' : shows x "\"" | x <- exCases]) ++ "\n];"
  hPutStr out $ unlines ["fromString " ++ shows x " = " ++ showResultSOME (checkOverflow width x) | x <- exCases]
  hPutStrLn code $ "List.app (fn s => (print (\"scan BIN \" ^ s ^ \" = \"); callO (StringCvt.scanString (" ++ name ++ ".scan StringCvt.BIN)) s))\n[" ++ concat (intersperse "\n," ['"' : showSigned showBin 0 x "\"" | x <- exCases]) ++ "\n];"
  hPutStr out $ unlines ["scan BIN " ++ showSigned showBin 0 x " = " ++ showResultSOME (checkOverflow width x) | x <- exCases]
  hPutStrLn code $ "List.app (fn s => (print (\"scan OCT \" ^ s ^ \" = \"); callO (StringCvt.scanString (" ++ name ++ ".scan StringCvt.OCT)) s))\n[" ++ concat (intersperse "\n," ['"' : showSigned showOct 0 x "\"" | x <- exCases]) ++ "\n];"
  hPutStr out $ unlines ["scan OCT " ++ showSigned showOct 0 x " = " ++ showResultSOME (checkOverflow width x) | x <- exCases]
  hPutStrLn code $ "List.app (fn s => (print (\"scan HEX \" ^ s ^ \" = \"); callO (StringCvt.scanString (" ++ name ++ ".scan StringCvt.HEX)) s))\n[" ++ concat (intersperse "\n," ['"' : showSigned showHex 0 x "\"" | x <- exCases]) ++ "\n];"
  hPutStr out $ unlines ["scan HEX " ++ showSigned showHex 0 x " = " ++ showResultSOME (checkOverflow width x) | x <- exCases]

main = do
  args <- getArgs
  case args of
    "Int8" : codefile : outfile : _ ->
      withFile codefile WriteMode $ \codeh ->
      withFile outfile WriteMode $ \outh ->
      run "Int8" (Just 8) (randomRM (-2^7, 2^7-1)) (randomRM (-2^16, 2^16)) codeh outh
    "Int16" : codefile : outfile : _ ->
      withFile codefile WriteMode $ \codeh ->
      withFile outfile WriteMode $ \outh ->
      run "Int16" (Just 16) (randomRM (-2^15, 2^15-1)) (randomRM (-2^24, 2^24)) codeh outh
    "Int32" : codefile : outfile : _ ->
      withFile codefile WriteMode $ \codeh ->
      withFile outfile WriteMode $ \outh ->
      run "Int32" (Just 32) (randomRM (-2^31, 2^31-1)) (randomRM (-2^36, 2^36)) codeh outh
    "Int54" : codefile : outfile : _ ->
      withFile codefile WriteMode $ \codeh ->
      withFile outfile WriteMode $ \outh ->
      run "Int54" (Just 54) (randomRM (-2^53, 2^53-1)) (randomRM (-2^65, 2^65)) codeh outh
    "Int64" : codefile : outfile : _ ->
      withFile codefile WriteMode $ \codeh ->
      withFile outfile WriteMode $ \outh ->
      run "Int64" (Just 64) (randomRM (-2^63, 2^63-1)) (randomRM (-2^70, 2^70)) codeh outh
    "IntInf" : codefile : outfile : _ ->
      withFile codefile WriteMode $ \codeh ->
      withFile outfile WriteMode $ \outh ->
      run "IntInf" Nothing (randomRM (-2^192, 2^192)) (randomRM (-2^192, 2^192)) codeh outh
    _ -> hPutStrLn stderr "unexpected argument"

-- run as:
-- cabal run GenIntTest.hs Int8 int8.sml int8.stdout
-- cabal run GenIntTest.hs Int16 int16.sml int16.stdout
-- cabal run GenIntTest.hs Int32 int32.sml int32.stdout
-- cabal run GenIntTest.hs Int54 int54.sml int54.stdout
-- cabal run GenIntTest.hs Int64 int64.sml int64.stdout
-- cabal run GenIntTest.hs IntInf int-inf-rand.sml int-inf-rand.stdout
