module DefinicionPorComprehension where


--Suma de los cuadrados de los n primeros números---------------------------------------------------------------------------------------------
sumaDeCuadrados x = sum( map (^2) (take x [1..x]) ) --Solucion Propia no uso definicion de funciones
--Segunda solucion
sumaDeCuadrados1 :: Integer -> Integer
sumaDeCuadrados1 x = sum [n^2 | n <- [1..x]]
--Muchas funciones se pueden escribir usando comprehension en lugar de map o filter
 
 --Listas con un elemento replicado  ///Basicamente es la funcion replicate-----------------------------------------------------------------------
replica :: Int -> a -> [a]
replica n x = [x | _ <- [1..n]] -- _ significa que no nos importa lo que vayamos a extraer de la lista, 
                                   --así que en vez de escribir el nombre de una variable que nunca usaríamos,
                                   -- simplemente escribimos _. 

--Definir la funcion suma tal que suma n es la suma de los no primeros números---LA UTILIZAREMOS EN TRIANGULOS ARIMETICOS
--suma 3 == 6 ------(1+2+3)
suma n = sum [1..n]

--Triangulos aritméticos---------------------------------------------------------------------------------------------------------
{- 
Se forman de la siguiente manera
1
2 3
4 5 6
7 8 9 10
11 12 13 14 15
16 16 18 19 20 21
linea 4 == [7,8,9,10]  -}
linea n = [ suma(n-1)+1..suma n]

--Definir la funcion triangulo tal que (triangulo n) es el trángulo aritmético de altura n-----------------------
--Ejemplo: triangulo 3 == [[1],[2,3],[4,5,6]]
triangulo n = [linea m | m <- [1..n]]

--Números perfectos.-------------------------------------------------------------------------------------------------------------------- 
{-
Un número perfecto es un número natural que es igual a la suma de sus divisores propios positivos. 
Así, 6 es un número perfecto porque sus divisores propios son 1, 2 y 3; y 6 = 1 + 2 + 3. OJO: que no se suma a si mismo.
Lo devideremos en dos problemas, uno para obtener los factores de un numero y otro donde encontremos el numero perfecto en su
-}
factores n = [x | x <- [1..n], n `mod` x == 0]
perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], sum(init(factores x)) == x]

--Números abundantes--------------------------------------------------------------------------------------------------------------------
{-
Un número natural n se denomina abundante e si es menor que la suma de sus divisores propios
 1+2+3+4+6 = 16 > 12 
Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son
-}
abundante ::  Int -> Bool
abundante n = n < sum(init(factores n))
--Ahora para que regresa una lista
abundante1 :: Int -> [Int]
abundante1 n = [x | x <- [1..n], abundante x]

{-
Definir la función todosPares tal que (todosPares n) se verifica si todos los
números abundantes menores o iguales que n son pares. Por ejemplo,
todosPares 10 == True
todosPares 100 == True
todosPares 1000 == False
-}
todosPares :: Int -> Bool
todosPares n = and [even x| x <- abundante1 n ]
{-OJO ACA COMPADRE
*DefinicionPorComprehension> [x | x <- [1..10],even x]    --filter even [1..10]
[2,4,6,8,10]
*DefinicionPorComprehension> [even x| x<- [1..10]]
[False,True,False,True,False,True,False,True,False,True]
-}

--Proyecto Euler
--tal que (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores que n. Por ejemplo : euler1 10 == 23 
euler1 :: Integer -> Integer
euler1 n = sum[x | x <- [1..n-1], mod x 3 == 0 || mod x 5 == 0 ] --multiplo x 3 || multiplo x 5]
                 --where multiplo x y = mod x y == 0

--Números pares de una terna
--(numeroDePares t) es el número de elementos pares de la terna t. Por ejemplo: numeroDePares (3,6,4) == 2
numeroDePares :: (Int,Int,Int) -> Int
numeroDePares(a,b,c) = sum [1 | n <- [a,b,c], even n] 

--Producto escalar
--productoEscalar [1,2,3] [4,5,6] == 32
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar (xs) (ys) = sum[ x*y | (x,y)  <- zip xs ys] 

--Suma de pares de elementos consecutivos
--(sumaConsecutivos xs) es la suma de los pares de elementos consecutivos de la lista xs. 
--Por ejemplo: sumaConsecutivos [3,1,5,2] == [4,6,7]     --3+1 = 4 , 1+5 = 6, 5 + 2 = 7
sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = [x+y| (x,y) <- zip xs (tail xs)]

