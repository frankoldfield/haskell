module Practica6_1 where

-- Definir el tipo de dato

type Persona = String
type Libro = String

type BD = [(Persona,Libro)]

-- Funciones para extraer información

libros :: BD -> Persona -> [Libro]

libros bd p = [y | (x,y) <- bd, x == p]

lectores :: BD -> Libro -> [Persona]

lectores bd l = [x | (x,y) <- bd, y==l]

prestado :: BD -> Libro -> Bool

prestado bd = not.null.(lectores bd) -- Versión currificada

numPrestados :: BD -> Persona -> Int

numPrestados bd = length.(lectores bd)

-- Funciones para modificar la base de datos

realizarPrestamo :: BD -> Persona -> Libro -> BD

realizarPrestamo bd p l = (p,l):bd

devolverPrestamo :: BD -> Persona -> Libro -> BD

devolverPrestamo bd p l = [prestamo | prestamo <- bd, prestamo/=(p,l)]

-- Posible ampliación

type NumEjem = [(Libro,Int)]

miBD :: BD
miBD=[("Juan","Opiniones de un payaso"),("Luis","Ulises"),("Pedro","Los misterios de Madrid"),("Juan","1984")]

misEjem :: NumEjem
misEjem = [("Opiniones de un payaso", 1), ("Ulises",7),("Los misterios de Madrid",0),("1984",3)] 

catalogadoLibro :: NumEjem -> Libro -> Bool

catalogadoLibro me l = (not.null) [(x,y) | (x,y) <- me, x==l]

disponibleLibro :: NumEjem -> Libro -> Bool

disponibleLibro me l = (not.null) [(x,y) | (x,y) <- me, x==l, y>0]

nuevoEjemplar :: NumEjem -> Libro -> NumEjem

nuevoEjemplar me l = if (catalogadoLibro me l) then (map addEjemplar me) else ((l,1):me) --Esta es mi solución
    where addEjemplar (l', c') = if (l'==l) then (l',c'+1) else (l',c') 

nuevoEjemplar2 :: NumEjem -> Libro -> NumEjem -- Esta es la solución dada

nuevoEjemplar2 ne l = if (catalogadoLibro ne l) then  [(x,if x==l then y+1 else y)| (x,y) <- ne] else ne++[(l,1)]

devuelveEjemplar,extraeEjemplar :: NumEjem -> Libro -> NumEjem

devuelveEjemplar ne l = [(x,if x==l then y+1 else y)| (x,y) <- ne]

extraeEjemplar ne l = if (disponibleLibro ne l) then [(x,if x==l then y-1 else y)| (x,y) <- ne] else error("No hay ejemplares de "++l)

-- Modificación realizarPrestamo y devolverPrestamo

realizarPrestamo2 :: BD -> NumEjem -> Persona -> Libro -> (BD,NumEjem)

realizarPrestamo2 bd ne p l = if (disponibleLibro ne l) then (realizarPrestamo bd p l, extraeEjemplar ne l) else error("No hay ejemplares de "++l) 

devolverPrestamo2 bd ne p l = if prestado then ([prestamo | prestamo <- bd, prestamo /= (p,l)],devuelveEjemplar ne l) else (bd,ne)
    where prestado = elem (p,l) bd