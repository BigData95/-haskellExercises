module ConsultaBD where


--Base de datos------------------------------------------------------------------------------------------------------------------------------------
personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
    ("Velazquez","Pintura",1599,1660),
    ("Picasso","Pintura",1881,1973),
    ("Beethoven","Musica",1770,1823),
    ("Poincare","Ciencia",1854,1912),
    ("Quevedo","Literatura",1580,1654),
    ("Goya","Pintura",1746,1828),
    ("Einstein","Ciencia",1879,1955),
    ("Mozart","Musica",1756,1791),
    ("Botticelli","Pintura",1445,1510),
    ("Borromini","Arquitectura",1599,1667),
    ("Bach","Musica",1685,1750)]
  
    
--Definir la función nombres tal que (nombres bd) es la lista de los nombres de las personas de la base de datos bd.------------------------------------------------------------------    
nombres :: [(String,String,Int,Int)] ->  [String]
nombres bd = [x| (x,_,_,_) <- bd]  --bd Es el nombre de la base de datos, que en este caso es una lista. 

--Definir la función musicos tal que (musicos bd) es la lista de los nombres de los músicos de la base de datos bd
musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [ x | (x,m,_,_) <- bd, m == "Musica"]

--Definir la función seleccion tal que (seleccion bd m) es la lista de los nombres de las personas de la base de datos bd cuya actividad es m
seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd xs = [x | (x,m,_,_) <- bd , m == xs]

--Definir la función vivas tal que (vivas bd a) es la lista de los nombres de las personas de la base de datos bd que estaban vivas en el año a
vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas bd a = [x | (x,_,y,z) <- bd , (a >= y && a<=z) ]
