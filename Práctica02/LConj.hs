{-
- Logica Conmputacional 2020-2
- Implementación del Conjunto.
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module LConj where

-- | equivL. Función que determina si dos conjuntos son equivalentes.
equivL :: Eq a => [a] -> [a] -> Bool
equivL l1 l2 = and [(length l1) == (length l2),  (length l1) == (length (inter l1 l2))]

-- | filtro. Función que dado un predicado y una lista l, devuelve
-- la lista de los elementos de l que cumplen con el predicado.
filtro :: (a -> Bool) -> [a] -> [a]
filtro p m = [x | x<-m, p(x)]

-- | diff. Función que devuelve la diferencia de listas.
diff :: Eq a => [a] -> [a] -> [a]
diff l1 l2 = [x | x<-l1, not (elem x l2)]

-- | union. Función que devuelve la union de listas.
union :: Eq a => [a] -> [a] -> [a]
union l m = [x | x<-l, not (elem x m)]++m

-- / union2. Funcion que devuelve la union de una lista de lista [[1],[2]] = [1,2]
union2 :: Eq a => [[a]] -> [a]
union2 l =[y | x<-l, y<-x] -- / No esta completo. No resvisa repeticiones

-- | inter. Función que devuelve la intersección de listas.
inter :: Eq a => [a] -> [a] -> [a]
inter l1 l2 =  [x |x<-l1, y<-l2, x==y]

-- / subconj. Función que devuelve la potencia de una lista
subconj :: [a] -> [[a]]
subconj  = error "aaaaaaaaaaaaaaaahhhhhhhhhhhhhhh!!!"
