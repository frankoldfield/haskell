module Practica5_1 where

--Ejercicio 1: ordenada

ordenada :: Ord a => [a] -> Bool

{-
-- Solución mala: Utilizo !!
ordenada a
    | length a <=2 = ((a !! 0) <= (a!!1))
    | otherwise = ((a !! 0) <= (a!!1)) && ordenada (drop 1 a)

-}

--Solución buena

ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = (x<=y) && ordenada (y:xs)

-- Ejercicio 2: borrar

borrar :: Eq a => a -> [a] -> [a]

borrar a (x:xs)
    | a==x = xs
    | otherwise = x : (borrar a xs)

-- Ejercicio 3: insertar

insertar :: Ord a => a -> [a] -> [a]
insertar e [] = [e]
insertar e (x:xs)
    | e<=x = (e:x:xs)
    | otherwise = x : (insertar e xs)

-- Ejercicio 4: ordInsercion

ordInsercion :: Ord a => [a] -> [a]

ordInsercion [] = []
ordInsercion (x:xs) = insertar x (ordInsercion xs)

-- Ejercicio 5: minimo

minimo :: Ord a => [a] -> a

minimo = head . ordInsercion

-- Ejercicio 6: mezcla ES ASÍ PORQUE XS E YS ESTÁN YA ORDENADAS

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla xs [] = xs
mezcla [] ys = ys
mezcla (x:xs) (y:ys)
    | x<=y = x:(mezcla xs (y:ys))
    | otherwise = y:(mezcla (x:xs) ys)

-- Ejercicio 7: mitades

mitades :: [a] -> ([a], [a])

mitades xs = (take n xs, drop n xs) where
    n = div (length xs) 2

-- Ejercicio 8: ordMezcla

ordMezcla :: Ord a => [a] -> [a]

ordMezcla [] = []
ordMezcla [a] = [a]

ordMezcla xs = mezcla (ordMezcla h1) (ordMezcla h2) where
    (h1,h2) = mitades xs

-- Ejercicio 9 esPermutacion :: Eq a => [a] -> [a] -> Bool

esPermutacion xs ys = ordMezcla xs == ordMezcla ys