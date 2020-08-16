module DefinicionesRecursivas where

--Potencial de exponente natural
potencia :: Integer -> Integer -> Integer
potencia a 0 = 1
potencia a b = a * (potencia a (b-1))

--Replicar por recursion
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar a b = b : (replicar (a-1) b)
--Con listas comprehension
replicarList :: Int -> a -> [a]
replicarList a b = [b| _ <- [1..a]]

{-Doble Factorial
0!! = 1
1!! = 1
n!! = n*(n-2)* ... * 3 * 1, si n es impar
n!! = n*(n-2)* ... * 4 * 2, si n es par
-}
dobleFactorial :: Int -> Int
dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial n = n*dobleFactorial(n-2)

{-Algoritmo de Euclides Maximo comun divisor
mcd(a, b) => a, si b = 0
             mcd(b, a módulo b), si b > 0
-}
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b ( a `mod` b)

--Pertenencia a una lista 
elemento :: Eq a => a -> [a] -> Bool
elemento x [] = False
elemento x (y:ys)   | x == y = True
                    | otherwise = elemento x ys
                    
--Ultimo elemento de una lista
last1 :: [a] -> a
last1 [x] = x
last1 (x:xs) = last1(xs)   

--Concatenacion de una lista
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1(xs) 

--Seleccion de un elemento
selecciona :: [a] -> Int -> a
selecciona (x:xs) 0 = x
selecciona (x:xs) n = selecciona xs (n-1)

--Seleccion de los primeros elementos
take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 _ [] = []
take1 n (x:xs) = x : take1 (n-1) xs 

--Intercalacion de la media artimetica
refinada :: [Float] -> [Float]
refinada (x:y:xs) = x : (x+y)/2 : refinada (y:xs)
refinada xs = xs

--Mezcla de listas ordenadas
mezcla :: Ord a => [a] -> [a] ->[a]
mezcla [] xs = xs
mezcla xs [] = xs
mezcla (x:xs) (y:ys) | y < x =  y : mezcla (x:xs) ys
                     | x <= y = x : mezcla xs (y:ys)  --otherwise=

--Mitades de una lista. Parte la lista por la mitad
mitades :: [a] -> ([a],[a])
mitades xs = (take (length xs `div` 2) xs , drop (length xs `div` 2) xs)   
--mitades xs = splitAt (length xs `div` 2) xs

{- Ordenacion por mezcla
tal que (ordMezcla xs) es la lista obtenida ordenando xs por mezcla (es decir, considerando
que la lista vacía y las listas unitarias están ordenadas y cualquier otra lista se ordena mezclando
las dos listas que resultan de ordenar sus dos mitades por separado). Por ejemplo,
ordMezcla [5,2,3,1,7,2,5] => [1,2,2,3,5,5,7]
-}    
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla ys) (ordMezcla zs)
                where (ys,zs) = mitades xs

--Verificar si esta ordenada
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs)  = x <= y && ordenada (y:xs)         