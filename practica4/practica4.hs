-- Clase racionales

type Racional1 = (Int, Int)

simplificaRac :: Racional1 -> Racional1

simplificaRac (n,d) = ((signum d)*(div n m), div d m)
    where m = gcd n d


multRac, divRac, sumRac, resRac :: Racional1 -> Racional1 -> Racional1

multRac (n1,d1) (n2,d2) = simplificaRac (n1*n2, d1*d2)

divRac (n1,d1) (n2,d2) = multRac (n1, d1) (d2,n2)

sumRac (n1,d1) (n2,d2) = (n3, d1*d2)
    where n3 = n1*d2 + n2*d1

resRac (n1,d1) (n2,d2) = sumRac (n1, d1) (n2, negate d2)

muestraRac :: Racional1 -> String

muestraRac (n,d) = show n1 ++ "/" ++ show d1
    where (n1,d1) = simplificaRac(n,d)

-- Segunda representación de racionales

data Racional2 = Rac Integer Integer
    deriving (Eq)

simplificaRac2 :: Racional2 -> Racional2

simplificaRac2 (Rac n d) = (Rac ((signum d)*(div n m)) (div d m))
    where m = gcd n d

multRac2, divRac2, sumRac2, resRac2 :: Racional2 -> Racional2 -> Racional2

multRac2 (Rac n1 d1) (Rac n2 d2) = simplificaRac2 (Rac (n1*n2) (d1*d2))

divRac2 (Rac n1 d1) (Rac n2 d2) = multRac2 (Rac n1 d1) (Rac d2 n2)

sumRac2 (Rac n1 d1) (Rac n2 d2) = (Rac (n3) (d1*d2))
    where n3 = n1*d2 + n2*d1

resRac2 (Rac n1 d1) (Rac n2 d2) = sumRac2 (Rac n1 d1) (Rac n2 (negate d2))

muestraRac2 :: Racional2 -> String

muestraRac2 (Rac n d)  = show n1 ++ "/" ++ show d1
    where (Rac n1 d1) = simplificaRac2(Rac n d)

instance Show Racional2 where
    show (Rac n 1) = show n 
    show (Rac n d ) = show n' ++ "/" ++ show d'
		where (Rac n' d') = simplificaRac2 (Rac n d)

instance Num Racional2 where
    (*) = multRac2
    (+) = sumRac2
    (-) = resRac2
    negate (Rac n1 d1) = simplificaRac2 (Rac (-n1) d1)
    fromInteger x = (Rac x 1)
    signum (Rac n d) = if (n*d)<0 then -1 else 1 
    abs (Rac n d) = if (n*d)<0 then simplificaRac2(Rac (-n) d) else simplificaRac2(Rac n d)

-- Naturales

data Nat = Cero | Succ Nat
    deriving (Eq)

instance Num Nat where
    n + Cero = n
    n + Succ m = Succ (n+m)
    n * Cero = Cero
    n * Succ m = n*m + n
    abs n = n
    signum Cero = Cero
    signum n = Succ Cero
    n - Cero = n
    Cero - Succ m = Cero  
    Succ n - Succ m = n - m
    fromInteger n
        | n<=0 = Cero
        | otherwise = Succ (fromInteger (n-1))

instance Ord Nat where
    Cero < Cero = False
    Cero < Succ m = True
    Succ m < Cero = False
    Succ n < Succ m = (n<m)

divModN :: Nat -> Nat -> (Nat,Nat)

divModN nat1 nat2
    | nat2 == Cero = undefined
    | nat1<nat2 = (Cero, nat2)
    | otherwise = (Succ c, m)
        where (c,m) = divModN (nat1-nat2) nat2


{-
--Esta declaración de instancia sería equivalente a poner la cláusula 'deriving Show'. (POR ESO DEFINIMOS UNA PROPIA)

instance Show Nat where 
     show Cero = "Cero"
     show (Succ Cero) = "Succ Cero" --Para evitar paréntesis
     show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"
-}


instance Show Nat where
    show Cero = show "0"
    show (Succ m) = show (read (show m) + 1)