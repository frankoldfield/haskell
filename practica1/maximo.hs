maximo, maximo' :: (Integer, Integer) -> Integer

maximo (x,y) = if x >= y then x else y

maximo' (x,y)
    | x<y = y
    | otherwise = x