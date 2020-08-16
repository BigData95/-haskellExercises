module DefinicionesRecursionYComprension where

--Suma de los cuadrados de los primeros números------------------------------------------------------------------------------------------------------------------------------
sumaCuadrados :: Integer -> Integer
sumaCuadrados n = sum[x^2 | x <- [1..n] ]
--Con map
sumaCuadrados1 n = sum(map (^2) [1..n])
--Recursiva
sumaCuadrados2 0 = 0
sumaCuadrados2 n = n^2 + sumaCuadrados2(n-1)

--Lista de los dígitos de un número
digitos ::Integer -> [Integer]
digitos n = reverse (digitos1 n)
digitos1 n 
    | n < 10 = [n]
    | otherwise = (n `rem` 10) : digitos1 (n `div` 10)
--COMPRENSION CON read y CON show
digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n ]

--Numeros pares de una lista-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pares xs = [x| x <- xs, even x]
pares1 xs = filter even xs

--Mitades de los pares-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mitadParesC :: [Integer] -> [Integer]
mitadParesC xs = [x `div` 2 | x <- xs , even x]

--Descomposición en productos de factores primos---------------------------------------------------------------------------------------------------------
--Sacamos los factores/ los numeros que lo dividen
factores n = [x| x <- [1..n], mod n x == 0]
--Decir si es un numero primo bool
primo :: Integer -> Bool
primo n = factores n == [1,n]
--Descomposicion
factoresPrimos :: Integer -> [Integer]
factoresPrimos n = [x | x <- factores n , primo x]
