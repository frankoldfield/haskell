cuadrado :: Integer -> Integer

cuadrado(x) = x*x

area :: Integer -> Float

area(r) = fromInteger (cuadrado(r)) * pi

cuadrado' :: Float -> Float

cuadrado'(x) = x * x

area' :: Float -> Float

area'(r) = pi * cuadrado'(r)