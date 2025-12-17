module Rosadelfa where

data RAdelfa a = Nodo a [RAdelfa a]

instance (Show a)=> Show (RAdelfa a) where
    show (Nodo d xs) = shown 0 (Nodo d xs)

espacios :: Int -> String
espacios 0 = ""
espacios n = " "++ espacios (n-1)

shown :: (Show a) => Int -> RAdelfa a -> String

shown n (Nodo x []) = espacios n ++ show x
shown n (Nodo d xs) = espacios n ++ show d++"\n"++ concat (map ((++"\n").(shown (n+1)))  xs)

podar :: (Show a) => RAdelfa a -> RAdelfa a

podar (Nodo x (t:ts)) = (Nodo x ts)



--Esta es mía pero me he sobrecomplicado pensando que las Rosadelfas tienen orden
{-
aplanar :: (Show a) => RAdelfa a -> [a]
aplanar (Nodo x []) = [x] 
aplanar (Nodo x (t:ts)) = (aplanar t) ++ [x] ++ concat (map f ts)
    where f y = aplanar y
-}

aplanar :: RAdelfa a -> [a]
aplanar (Nodo x xts) = x:concat (map aplanar xts)

