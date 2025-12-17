module ArbolBinario where

data ArbolB a = Hoja
    | Rama (ArbolB a) a (ArbolB a)
    deriving Show


arbol = Rama 
            (Rama 
                (Rama Hoja 1 Hoja) 
                2 
                (Rama Hoja 3 Hoja))
            4 
            (Rama 
                (Rama Hoja 5 Hoja) 
                6 
                (Rama Hoja 7 Hoja))

tama :: ArbolB a -> Int

tama Hoja = 0
tama (Rama Hoja x Hoja) = 1 --Innecesario
tama (Rama b x c) = 1 + (tama b) + (tama c)

aplanar :: ArbolB a -> [a]

aplanar Hoja = []
aplanar (Rama b x c) = (aplanar b)++[x]++(aplanar c)

pertenece :: (Eq a) => a -> ArbolB a -> Bool --Mi solución
pertenece x tree = elem x (aplanar tree) 

pertenece2 :: (Ord a) => a -> ArbolB a -> Bool --Solución dada
pertenece2 e Hoja = False
pertenece2 e (Rama iz x der)
    | x==e = True
    | e<x = pertenece2 e iz
    | e>x = pertenece2 e der
    
insertar :: (Ord a) => a -> ArbolB a -> ArbolB a

insertar e Hoja = (Rama Hoja e Hoja)

insertar e (Rama iz x der)
    | e<x = (Rama (insertar e iz) x der)
    | otherwise = (Rama iz x (insertar e der))

borrar :: (Ord a) => a -> ArbolB a -> ArbolB a

borrar e Hoja = Hoja
borrar e (Rama iz x der)
    | e<x = (Rama (borrar e iz) x der)
    | e>x = (Rama iz x (borrar e der))
    | vacio der = iz
    | vacio iz = der
    | otherwise = juntar iz der

vacio :: ArbolB a -> Bool
vacio Hoja = True
vacio _ = False

juntar :: Ord a => ArbolB a -> ArbolB a -> ArbolB a

juntar iz der = (Rama iz minimoD (borrar minimoD der))
    where minimoD = minArbol der

minArbol :: (Ord a) => ArbolB a -> a
minArbol (Rama iz x der)
    | vacio iz = x
    | otherwise = minArbol iz

crearArbolL :: (Ord a) => [a] -> ArbolB a

crearArbolL [x] = (Rama Hoja x Hoja)
crearArbolL (x:xs) = insertar x (crearArbolL xs)

--Solución con foldr
crearArbolL2 :: (Ord a) => [a] -> ArbolB a
crearArbolL2 l = foldr insertar Hoja l -- Se puede poner tanto con la l como point-free (sin la l)

ordenarConArbol :: (Ord a) => [a] -> [a]

ordenarConArbol = aplanar.crearArbolL

-------------- 2. Conjuntos con listas ----------------

-- subconjunto

subconjunto :: Eq a => [a] -> [a] -> Bool

subconjunto [] _ = True
subconjunto (x:xs) ys = (elem x ys) && (subconjunto xs ys)

igualesC :: Eq a => [a] -> [a] -> Bool
igualesC xs ys = (subconjunto xs ys) && (subconjunto ys xs)