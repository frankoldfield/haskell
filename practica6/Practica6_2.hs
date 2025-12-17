---------------------------------------------------------------------------
--CIFRADO DEL CESAR
---------------------------------------------------------------------------

--Librer�as usadas:
 
import Data.Char
import Data.List
import System.IO

--1. Renombramiento:

type Mensaje = String

--2.
 
minusculaAint :: Char -> Int

minusculaAint c = ord c - ord 'a'

mayusculaAint :: Char -> Int

mayusculaAint c = ord c - ord 'A'

intAminuscula :: Int -> Char

intAminuscula n = chr (ord 'a'+n )

intAmayuscula :: Int -> Char

intAmayuscula n = chr (ord 'A'+n )

--3.

desplaza :: Int -> Char -> Char

desplaza n c
    | elem c ['a'..'z'] = intAminuscula (mod (minusculaAint c+n) 26)
    | elem c ['A'..'Z'] = intAmayuscula (mod (mayusculaAint c+n) 26)
    | otherwise = c

--4.
codifica :: Int -> String -> String

codifica n ms = map (desplaza n) ms -- Mi forma

codifica2 :: Int -> String -> String

codifica2 n ms = [desplaza n x | x <- ms ] -- Por comprensión

--5.

porcentaje :: Int -> Int -> Float

porcentaje n m = (fromIntegral n / fromIntegral m) * 100

-- 6.

letras :: String -> String

letras ms = [x | x<-ms, elem x (['a'..'z']++['A'..'Z'])]

-- 7.

ocurrencias :: Char -> String -> Int

ocurrencias c ms = length [x | x<-ms, c==x]

-- 8.

frecuencias :: String -> [Float]

frecuencias ms = [porcentaje ((ocurrencias x ms) + (ocurrencias y ms)) len  | (x,y) <- zip ['a'..'z']['A'..'Z']] -- Mi solución usando zip
    where len = length (letras ms)

frecuencias2 :: String -> [Float] --Solución dada usando toLower
frecuencias2 xs = 
    [porcentaje (ocurrencias x xs') n | x <- ['a'..'z']]
    where xs' = [toLower x | x <- xs]
          n   = length (letras xs)

-- 9.

chiCuad :: [Float] -> [Float] -> Float

chiCuad os es = sum [ (x-y)^2/y | (x,y) <- zip os es ]

-- 10.

rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- 11. 

tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]


descifra :: Mensaje -> Mensaje
descifra xs =  codifica (-factor) xs
 where
  factor = head (posiciones (minimum tabChi) tabChi)
  tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
  tabla' = frecuencias xs
 
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = 
    [i | (x',i) <- zip xs [0..], x == x']
