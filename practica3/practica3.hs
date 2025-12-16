module Main where
import Test.HUnit
import Test.QuickCheck
import Pictures

-- Ejemplo
{-
-- Funciones que se van a probar
f1 :: Int -> (Int, Int)
f1 x = (1, x)
f2 :: Int -> (Int, Int)
f2 v = (v+2, v+3)
f3 :: Int -> Bool
f3 v = (v > 5)
f4 :: Int -> String -> String
f4 n s = if (f3 n) then "" else s
--Creación de los Test
test1,test2,test3,test4 :: Test
test1 = TestCase (assertEqual "Fallo con (f1 3)." (1,3) (f1 3))
test2 = TestCase (assertEqual "Fallo en primer elemento de f2 3." 5 (fst(f2 3)))
test3 = TestCase (assertBool "Fallo de prueba con f3." (f3 (snd(f2 3))))
test4 = TestCase (assertString (f4 6 "Resultado incorrecto con f4."))
tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3,TestLabel "test4" test4]
--Ejecución del test
main :: IO Counts
main = runTestTT tests
-}
-- Ejercicio 1: chessG y tablero

-- chessG

blackWhite :: Integer -> Picture -> Picture -> Picture

blackWhite m b w
    | m<=1 = b
    | otherwise = beside b (whiteBlack (m-1) b w) 

whiteBlack :: Integer -> Picture -> Picture -> Picture

whiteBlack m b w
    | m<=1 = w
    | otherwise = beside w (blackWhite (m-1) b w) 

chessBlack :: Integer -> Integer -> Picture -> Picture -> Picture

chessBlack n m b w
    | n<=1 = blackWhite m b w
    | otherwise = above (blackWhite m b w ) (chessWhite (n-1) m b w)

chessWhite :: Integer -> Integer -> Picture -> Picture -> Picture

chessWhite n m b w
    | n<=1 = whiteBlack m b w
    | otherwise = above (whiteBlack m b w ) (chessBlack (n-1) m b w)

chess :: Integer -> Picture -> Picture -> Picture

chess n b w = chessWhite n n b w

chessG :: Picture -> Picture -> Picture

chessG b w = chess 3 b w

-- tablero
filaBlanca :: Integer -> Picture -> Picture

filaBlanca m w
    | m<=1 = w
    | otherwise = beside w (filaBlanca (m-1) w)

casillaBlanca :: Integer -> Integer -> Picture -> Picture

casillaBlanca n m w
    | n<=1 = filaBlanca m w
    | otherwise = above (filaBlanca m w) (casillaBlanca (n-1) m w) 

filaNegra :: Integer -> Picture -> Picture

filaNegra m b
    | m<=1 = b
    | otherwise = beside b (filaNegra (m-1) b)

casillaNegra :: Integer -> Integer -> Picture -> Picture

casillaNegra n m b
    | n<=1 = filaNegra m b
    | otherwise = above (filaNegra m b) (casillaNegra (n-1) m b) 

tablero :: Integer -> Picture -> Picture -> Picture

tablero n b w = chess 3 (casillaNegra n n b) (casillaBlanca n n w)

-- Ejercicio 2

testbase, testInvertido, testEsp :: Test

testbase = TestCase (assertBool "Test base ha fallado" ((tablero 1 white black) == chessG white black))

testInvertido = TestCase (assertBool "Test invertido ha fallado" ( (tablero 1 white black) == (invertColour (tablero 1 black white ))))

testEsp = TestCase (assertBool "Test giro ha fallado" (( invertColour (tablero 1 black white)== flipH (tablero 1 black white)) && ( invertColour (tablero 1 black white)== flipV (tablero 1 black white))))

tests :: Test
tests = TestList [TestLabel "testbase" testbase, TestLabel "testInvertido" testInvertido, TestLabel "testEsp" testEsp]

main :: IO Counts
main = runTestTT tests

-- Ejercicio 3: QuickCheck

prop_abV :: Picture -> Picture -> Bool

prop_abV pic1 pic2 = (flipV  (above pic1 pic2)) == above (flipV pic1) (flipV pic2) 

prop_abH :: Picture -> Picture -> Bool

prop_abH pic1 pic2 = (flipH  (above pic1 pic2)) == above (flipH pic1) (flipH pic2) 