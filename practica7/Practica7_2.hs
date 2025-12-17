module Practica7_2 where

import Rosadelfa

type Valor = (Integer,Integer)

type Adelfa = RAdelfa Valor

godel :: Integer -> Integer

godel n = product (zipWith (^) primos (digitos n))

primos :: [Integer]
primos = [p | p <-[2..], primo p ]

divisible :: Integer -> Integer -> Bool
divisible x y = (rem x y) == 0

divisores :: Integer -> [Integer]
divisores x = [y | y <- [1..x], divisible x y ]

primo :: Integer -> Bool
primo x = (divisores x)==[1,x]

digitos :: Integer -> [Integer]
digitos n
    | n==0 = []
    | otherwise = (digitos (div n 10)) ++ [rem n 10]

-- Esta es mi solución:
{-
pots :: Integer -> [Integer]
pots n = [n^m | m <- [1..]]
-}

-- Esta es la solución oficial:
pots :: Integer -> [Integer]
pots p = iterate (p*) 1

gArbol :: Int -> Adelfa
-- (0, 2^0) -> [(1, 2^0),...]
gArbol k = podar (construirArbol k (0,1))

construirArbol :: Int -> Valor -> Adelfa
{-


construirArbol 1 (n,g) = ((n,g) ++ concat[ (((y++[d]),godel y) []) | d <- [0..9] ])
    where y = digitos n
construirArbol k (n,g) = (n,g) ++ concat(construirArbol (k-1) (x,godel x))
-}
construirArbol 0 (n,g) = Nodo (n,g) []
construirArbol k (n,g) = Nodo (n,g) [ construirArbol (k-1) ((listaAEntero (y++[d])),godel (listaAEntero (y++[d])))  | d <- [0..9] ]
    where y = digitos n

listaAEntero :: [Integer] -> Integer
listaAEntero = foldl (\acc d -> acc * 10 + d) 0

meertens :: Int -> [Integer]
meertens k = [n | (n,g) <- aplanar (gArbol k), n==g]

