module Main where



main::IO()
main = undefined



--Progresion Numerica------------------------------------------------------------------------
--Dados un numero inicial, un paso y un numero de elemnto de una progresion obtenemos
--10,3,5     -->  [10,13,16,19,22]
--Solucion recursiva
progresion ini paso 1 = [ini] --Caso base
progresion ini paso n = ini:progresion (ini+paso) paso (n-1)

--Ultima cifra de un numero-------------------------------------------------------------------
ultima x = mod x 10
--2 `mod` (-3)  ==  -1
--2 `rem` (-3)  ==  2

--Maximo de 3 numero----------------------------------------------------------------------
maximo a b c = max a (max b c) --Curry

--XOR-----------------------------------------------------------------------------------------------------------------
--Definir la función xor1 que calcule la disyunción excluyente a partir de la tabla
--de verdad. Usar 4 ecuaciones, una por cada línea de la tabla.
xor1 True True = False
xor1 True False = True
xor1 False True = True
xor1 False False = False
--Distinta solicion --Propuesta por mi---
xor2 :: Bool -> Bool -> Bool
xor2 a b = if(a == b) then False
            else True
--Distinta solucion Propuesta por mi---
xor3 :: Bool -> Bool -> Bool
xor3 a b 
    | (a == b) = False
    | (a /= b) = True
    |(otherwise) = False
--Distinta solucion, mi favorita---
xor4 x y = x /= y
--La mas compleja---
xor5 x y = (x || y) && not (x && y)
--Clever---
xor6 True y = not y
xor6 False y = y

--Primer elemento al final de la lista------------------------------------------------------------
--tal que (rota1 xs) es la lista obtenida poniendo el primer elemento de xs al final de la lista. Por ejemplo
primera xs = tail xs ++ [head xs]

--Extra n elementos y los pone al final----------------------------------------------------------
extrae n xs = drop n xs ++ take n xs --take extra elementos n de la lista, drop quita n elementos de la lista


--Una lista que contiene solamente el elemento mas grande y el menor de otra lista-----------------------------
mayormenor xs = [maximum xs] ++ [minimum xs]
--otra solucion----
mayormenor1 xs = [maximum xs, minimum xs]

--Reconocimiento de palindromos de listas--------------------------------------------------------------------------------------------------------------------
palindromo xs = xs == reverse xs  --Expresion booleana que regresa True o False


--Interior lista--------------------------------------------------------------------------------------------------------------------
--Ejercicio 1.11.1. Definir la función interior tal que (interior xs) es la lista obtenida eliminando los extremos de la lista xs. Por ejemplo,
--interior [2,5,3,7,3] == [5,3,7]
--interior [2..7] == [3,4,5,6]
interior xs = tail(init xs)
interior1 xs = init(tail xs)


--Elementos finales de una lista --------------------------------------------------------------------------------------------------------------------
--finales 3 [1,2,3,4,5,6,7,8] == [6,7,8]
--drop 3 [1,2,3,4,5,6,7,8] == [4,5,6,7,8]
--take 3 [1,2,3,4,5,6,7,8] == [1,2,3]
finales n xs = drop(length xs - n) xs

--Segmentos de una lista------------------------------------------------------------------------
--Definir la función segmento tal que (segmento m n xs) es la lista de los
--elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
--segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
--segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
--segmento 5 3 [3,4,1,2,7,9,0] == []
segmento m n xs = drop(m-1) (take n xs)

---Invertir lista----------------------------------------------------------------------------------------------------------
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

--QuickSort-------------------------------------------------------------------------------------
qsort[] = []      --Comprension de lista. Utilizamos el primer elemento de una lista como pivote
qsort (h:t) = qsort [ x | x <- t , x <= h] ++
                    [h] ++ qsort [x | x <- t, x > h]
                    
--Criba de EErastostenes------------------------------------------------------------------
sieve (p: lis) = p: sieve[n | n <- lis, mod n p /= 0]
primes = sieve[2..]   
----EJemplo:  take 100 primes
            

--Extremos de una lista--------------------------------------------------
--Es la lista formada por los n  elementos principales y n elementos finales              
extremos n xs = (take n xs) ++ (finales n xs)  --finales n xs = drop(length xs - n) xs
--Version 2 
extremos1 n xs = (take n xs) ++ (take n (reverse xs))


--Mediano de 3 numeros------------------------------------------------------------
--Es el numero mediano de los 3 numeros dados
mediano a b c = a + b + c - minimum[a,b,c] - maximum[a,b,c]
--Segunda solucion 
mediano1 a b c
       | (a < max && a > min) = a
       | (b < max && b > min) = b
       | otherwise = c
       where min = minimum[a,b,c]
             max = maximum[a,b,c]

--Igualdad de tres numeros. Son iguales los tres?---------------------------------------------------------------------------------
tresIguales a b c = (a == b && a == c)

--Diferencia de tres elementos. Los elementos son distintos?
tresDiferentes a b c = a /= b && a /=c && b/=c
   
--Propiedad triangular--------------------------------------------------------------------------
--En todo triangulo cada lado es menor que la suma de los otros dos y mayor que su diferencia
triangular a b c = (a < b+c && b < a+c && c < a+b)

--Tipo de triangulo---------------------------------------------------------------------------------------------------------------
--Equilatero = los tres lados son iguales
--Isosceles = dos lados iguales y otro desigual
--Escaleno = los tres lados son distintos 
tipoTriangulo1 a b c 
            | (a == b)&&(b == c) = putStrLn "Equilatero"
            | ((a == b)&&(a /= c || b /= c)) || ((a == c)&&( a /= b || c /= b)) 
                    || ((b == c)&&(b/=a || c /= a)) = putStrLn "Isosceles"
            |(a /= b && a /=c && b /= c) = putStrLn "Escaleno"
            
tipoTriangulo a b c = if (triangular a b c )
                        then tipoTriangulo1 a b c 
                        else putStrLn "No es un triangulo"
                      
--Division segura-------------------------------------------------------------------------------------------
--Tal que x/y si no es cero y 999 en caso contrario 
divisionSegura _ 0 = 999
divisionSegura x y = x/y
divisionSegura1 x y = if x==0 then 999 else x/y

--Rectangulo de área máxima-----------------------------------------------
mayorRectangulo (a,b) (x,y) = if(a*b > x*y) then (a,b)
                                else (x,y)
mayorRectangulo1 (a,b) (x,y) | a*b > x*y = (a,b)
                             | otherwise = (x,y) 
                             
--Puntos del plano-----------------------------------------------------------------------------
cuadrante (x,y)
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = 0
    
-- Mayor número de 2 cifras con dos dígitos dados(OJO que son digitos y no número)---------------------------------------------
numeroMayor x y = mayor*10 + menor
        where mayor = max x y --maximum [x,y] 
              menor = min x y --minimum [x,y]
                              