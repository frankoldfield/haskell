module Montones (Monton (..), Tablero, xor, sumNim) where

-------------------- A -----------------------
-- Ejercicio 1

data Monton a = Mt [a]

type Tablero = Monton Int

type Binario = [Int]

type Binarios = Monton Binario

-- Ejercicio 2

instance Show Tablero where
    show :: Tablero -> String
    show (Mt t) = concat [show f ++ " ->" ++ concat (replicate x " *") ++ "\n"  | (f,x) <- zip [1..]t ]

instance Show Binarios where
    show (Mt t) = concat [show f ++ " -> " ++ (muestraBin b) ++ "\n" | (f,b) <- zip [1..] t ]
        where muestraBin xs = concat [show x | x<-xs]

-- Ejercicio 3
-- Mi solución (Mas sencillita)
{-
entero2binario :: Int -> Binario
entero2binario 0 = [0]
entero2binario 1 = [1]
entero2binario n = entero2binario (n `div` 2) ++ [n `mod` 2]

-}

-- Solución dada con acumulador (xs), se va construyendo y pasando xs al siguiente paso, hasta que n es 0

entero2binario :: Int -> Binario
entero2binario 0 = [0]
entero2binario n = fst (f3 ([],n)) where f4 = (`mod` 2) -- f4 es una abreviatura para calcular el módulo base 2
                                         f3 (xs,n)
                                            | n==0 = (xs,n) -- Finalmente devuelve la tupla que contiene la lista en binario y n (este luego se descarta)
                                            | otherwise = f3 (f4 n:xs,div n 2) --Le añade el digito binario a la lista y sigue con lo que queda del número

-- Ejercicio 5

binario2entero :: Binario -> Int
binario2entero [] = 0
binario2entero (x:xs) = x*2^(length xs) + binario2entero xs

xor :: Int -> Int -> Int
xor a b = binario2entero [if b1/=b2 then 1 else 0| (b1,b2) <- zip (cerosIzda bl1) (cerosIzda bl2)]
                                            where bl1 = entero2binario a
                                                  bl2 = entero2binario b
                                                  cerosIzda l = replicate (lmax - length l) 0 ++ l
                                                  lmax = max (length bl1) (length bl2)

-- Ejercicio 6

sumNim :: [Int] -> Int
sumNim = foldr xor 0

-- Otra forma
sumNim' :: [Int] -> Int
sumNim' []     = 0              
sumNim' (x:xs) = x `xor` sumNim' xs 
