module DefinicionPorComprehension where


--Suma de los cuadrados de los n primeros números
sumaDeCuadrados x = sum( map (^2) (take x [1..x]) ) --Solucion Propia no uso definicion de funciones
--Segunda solucion
sumaDeCuadrados1 :: Integer -> Integer
sumaDeCuadrados1 x = sum [n^2 | n <- [1..x]]

 