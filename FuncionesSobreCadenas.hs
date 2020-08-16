module FuncionesSobreCadenas where
import Data.Char

--Capitalización de una cadena.
mayusculaInicial :: String -> String
mayusculaInicial [] = []
mayusculaInicial (x:xs) = toUpper x : [toLower x | x <- xs]

{-Título con las reglas de mayúsculas iniciales. 
La primera paralabra comienza con mayúscula
todas las palabras que tienen 4 letras como mínimo empiezan con mayúsculas.
 titulo ["eL","arTE","DE","La","proGraMacion"]  ------->  ["El","Arte","de","la","Programacion"]
-}
titulo :: [String] -> [String]
titulo [] = []
titulo (p:ps) = mayusculaInicial p : [transforma p | p <- ps]
transforma :: String -> String
transforma p | length p >= 4 = mayusculaInicial p
minuscula :: String -> String
minuscula xs = [toLower x | x <- xs]

{- Búsqueda en crucigramas
tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de la lista de palabras ps
que tienen longitud lon y poseen la letra l en la posición pos (comenzando en 0). Por ejemplo,
ghci> buscaCrucigrama 'c' 1 7 ["ocaso", "acabado", "ocupado"]
["acabado","ocupado"]
-}
buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama letra posicion longitud xs = 
                                            [x | x <- xs,
                                                         length x == longitud,
                                                         0 <= posicion, posicion < length x,
                                                         x !! posicion == letra]
                                                         
--Posiciones de un carácter en una cadena
posiciones :: String -> Char -> [Int]
posiciones xs c = [n | (x,n) <- zip xs [0..], x == c ]    

                                          