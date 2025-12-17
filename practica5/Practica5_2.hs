module Practica5_2 where

import Practica5_1

-- Ejercicio 1

cAplica :: (a->[b]) -> [a] -> [b]

cAplica f xs = concat (map f xs)

-- Ejercicio 2: rotaLista

rotaLista :: [a] -> [[a]]

rotaLista xs = rota xs (length xs)
    where
        rota _ 0 = []
        rota (x:xs) (n) = (x:xs) : rota (xs++[x]) (n-1)

-- Ejercicio 3: quitaCabeza

quitaCabeza :: Eq a => [a] -> [[a]] -> [[a]]

quitaCabeza [] listas = listas
quitaCabeza (x:xs) listas = quitaCabeza xs (limpia x listas)
    where
        limpia x [] = []
        limpia x ([]:ys) = limpia x ys
        limpia x (y:ys)
            | x == (head y) = limpia x ys
            | otherwise = y:(limpia x ys)

quitaCabeza2 :: Eq a => [a] -> [[a]] -> [[a]]

quitaCabeza2 xs xss = filter buena xss
    where
        buena [] = False
        buena (w:ws) = notElem w xs

-- Ejercicio 4: rotaTitulo

palTriviales = ["la","las","como", "de", "a", "con", "su","el", "un", "en"]

rotaTitulo :: String -> [String]

rotaTitulo tit = ( map unwords (quitaCabeza palTriviales (rotaLista (words tit) ) ) )

-- Ejercicio 5: kwic

kwic :: [String] -> [String]

kwic a = ordMezcla (cAplica rotaTitulo a)