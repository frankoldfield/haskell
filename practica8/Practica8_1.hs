--Para evitar que aparezcan los warnings debidos a tabuladores
{-# OPTIONS_GHC -fno-warn-tabs #-} 

module Practica8 where
import ArbolBinario
import Prelude

main = do
 	putStr "Fichero de entrada: "
 	fentrada <- getLine
 	cad <- readFile fentrada
 	putStr "Dato para percentil: "
 	p <- getLine
	putStr "El dato "
	putStr p	
	putStr " se encuentra en el percentil "
 	print (percentil (read p) (recogeDatos cad))  
	putStrLn " de la lista: "
 	putStrLn (show (recogeDatos cad))

recogeDatos :: String -> [Integer]
recogeDatos w = ordenarConArbol ( map read (words w))

menores :: Integer -> [Integer] -> [Integer]

menores x l = [m | m<-l, m<=x]

percentil :: Integer -> [Integer] -> Int

percentil x l = div (length (menores x l)*100) (length l)