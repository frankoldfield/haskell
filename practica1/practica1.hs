-- Ejercicio 1: Maximo
maximo, maximo' :: (Integer, Integer) -> Integer

maximo (x,y) = if x >= y then x else y

maximo' (x,y)
    | x<y = y
    | otherwise = x

-- Ejercicio 2: Área círculo

cuadrado :: Integer -> Integer

cuadrado(x) = x*x

area :: Integer -> Float

area(r) = fromInteger (cuadrado(r)) * pi

cuadrado' :: Float -> Float

cuadrado'(x) = x * x

area' :: Float -> Float

area'(r) = pi * cuadrado'(r)

-- Ejercicio 3: Fibonnaci

fib :: Integer -> Integer

fib (x)
    | x==0 = 1
    | x==1 = 1
    | x>1 = fib(x-1) + fib(x-2)

-- Ejercicio 4: Valor absoluto de un entero

absol :: Integer -> Integer

absol (x)
    | x<0 = negate(x)
    | x>=0 = x

-- Ejercicio 5: Aumentar y aumentar2

aumentar :: Integer -> Integer

aumentar (x) = cuadrado(x+1)

aumentar2 :: Integer -> Integer

aumentar2 (x) = aumentar(x) + 1

-- Ejercicio 6: Dar dos definiciones diferentes para la función nAnd

nAnd :: Bool -> Bool -> Bool

nAnd x y = not(x && y)

nAnd' :: Bool -> Bool -> Bool

nAnd' x y
    | x && y = False
    | otherwise = True

-- Ejercicio 7: xOr

xOr :: Bool -> Bool -> Bool

xOr x y = not(x == y)

xOr' :: Bool -> Bool -> Bool

xOr' x y
    | x==y = False
    | otherwise = True

-- Ejercicio 8: minimoTres
minimo :: Integer -> Integer -> Integer

minimo x y
    | x>y = y
    | otherwise = x


minimoTres :: Integer -> Integer -> Integer -> Integer

minimoTres x y z = minimo x (minimo y z)

-- Ejercicio 9: 

maximoTres :: Integer -> Integer -> Integer -> Integer

maximoTres x y z 
    | x>y = maximo(x,z)
    | otherwise = maximo(y,z)
-- En la solución no se usa la función maximo

-- Ejercicio 10: numeroCentral

numeroCentral :: Integer -> Integer -> Integer -> Integer

numeroCentral x y z = x+y+z-(maximoTres x y z) - (minimoTres x y z)
-- En la solución usan guardas y funciones auxiliares

-- Ejercicio 11: productoRango

productoRango :: Integer -> Integer -> Integer

productoRango m n
    | n<m = 0
    | n==m = m
    | n>m = n * productoRango m (n-1)

fact :: Integer -> Integer
fact 0 = 1
fact n = productoRango 1 n

-- Ejercicio 12: definición recursiva de multiplicación de naturales

prod:: Int -> Int -> Int

prod x y
    | x==0 || y==0 = 0
    | otherwise = x + (prod x (y-1)) 