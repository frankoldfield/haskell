import ImagenesSVG

-- Ejercicio 1: cuatroImg

-- Mi solución
cuatroImg :: Imagen -> Imagen

cuatroImg pic = encima (junto_a (pic) (giraV (invierte_color pic))) (junto_a (invierte_color pic) (giraV pic))

-- Solución dada
cuatroImg' :: Imagen -> Imagen

cuatroImg' pic =
	left `junto_a` right
		where
		 left = pic `encima` invierte_color pic
		 right = invierte_color  (giraV pic) `encima` giraV pic

-- Ejercicio 2: ajedrez

negroBlanco :: Integer -> Imagen
negroBlanco n
    | n<=1 = negro
    | otherwise = negro `junto_a` blancoNegro(n-1)

blancoNegro :: Integer -> Imagen
blancoNegro n
    | n<=1 = blanco
    | otherwise = blanco `junto_a` negroBlanco(n-1)

ajedrezNegro :: Integer -> Integer -> Imagen
ajedrezNegro n m
	| n <= 1 = negroBlanco m
	| otherwise = negroBlanco m `encima` ajedrezBlanco (n-1) m

ajedrezBlanco :: Integer -> Integer -> Imagen
ajedrezBlanco n m
	| n <= 1 = blancoNegro m
	| otherwise = blancoNegro m `encima` ajedrezNegro (n-1) m

-- Ejercicio 3: Tablero de ajedrez con caballos

negroBlanco' :: Integer -> Integer -> Imagen
negroBlanco' n m
    | m<=1 && m==(8-n+1) = negro'
    | m<=1 = negro
    | m==(8-n+1) = negro' `junto_a` blancoNegro' n (m-1)
    | otherwise = negro `junto_a` blancoNegro' n (m-1)

blancoNegro' :: Integer -> Integer -> Imagen
blancoNegro' n m
    | m<=1 && m==n = blanco'
    | m<=1 = blanco
    | m==n = blanco' `junto_a` negroBlanco' n (m-1)
    | otherwise = blanco `junto_a` negroBlanco' n (m-1)

ajedrezNegro' :: Integer -> Integer -> Imagen
ajedrezNegro' n m
	| n <= 1 = negroBlanco' n m
	| otherwise = negroBlanco' n m `encima` ajedrezBlanco' (n-1) m

ajedrezBlanco' :: Integer -> Integer -> Imagen
ajedrezBlanco' n m
	| n <= 1 = blancoNegro' n m
	| otherwise = blancoNegro' n m `encima` ajedrezNegro' (n-1) m

ajedrez' :: Imagen
ajedrez'= ajedrezBlanco' 8 8

blanco' = caballoPequeño 
negro' = invierte_color (giraV caballoPequeño)