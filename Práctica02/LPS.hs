{-
- Logica Conmputacional 2020-2 
- Practica02 Parte 1, Implementación del algoritmo dpll.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module LPS where

import LProp
import LConj

-- | Estado. Tipo que representa un estado de variables.
type Estado = [VarP]

-- | i. Implementación de la función de interpretación.
i :: Estado -> Prop -> Bool
i e p = error "Funcion a implementar"

-- | vars. Función que devuelve el conjunto de variables proposicionales de una
-- fórmula.
vars :: Prop -> [VarP]
vars p = error "Funcion a implementar"

-- | estados. Función que devuelve todos los posibles estados de una fórmula.
estados :: Prop -> [Estado]
estados p = error "Funcion a implementar"

-- | modelos. Función que devuelve todos los posibles modelos de una fórmula.
modelos :: Prop -> [Estado]
modelos p = error "Funcion a implementar"

-- | tautologia. Función que indica si una fórmula es una tautología.
tautologia :: Prop -> Bool
tautologia p = error "Funcion a implementar"

-- | satisfen. Función que determina si una fórmula es satisfacible en un
-- estado dado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = error "Funcion a implementar"

-- | satisf. Función que determina si una fórmula es satisfacible.
satisf :: Prop -> Bool
satisf p = error "Funcion a implementar"

-- | insatisfen. Función que determina si una fórmula es insatisfacible en un
-- estado dado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = error "Funcion a implementar"

-- | satisf. Función que determina si una fórmula es una contradicción.
contrad :: Prop -> Bool
contrad p = error "Funcion a implementar"

-- | equiv. Función que determina si dos fórmulas son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = error "Funcion a implementar"

-- | estadosConj. Función que dado un conjunto de formaulas, devuleva el 
-- conjunto de todos los posibles estados del conjunto de formulas. la funcion Union2 esta en el modulo LConj
estadosConj :: [Prop] -> [Estado]
estadosConj lp = error "Funcion a implementar"

-- | modelosConj. Función que dado un conjunto de fórmulas, devuelva el 
-- conjunto de todos los posibles modelos del conjunto de fórmulas.
modelosConj :: [Prop] -> [Estado]
modelosConj lp = error "Funcion a implementar"

-- | satisfenConj. Función que dada una interpretación y un conjunto de fórmulas, 
-- indique si el conjunto es satisfacible en el estado dado. 
satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj e lp = error "Funcion a implementar"

-- | satisfConj. Función que dado un conjunto de fórmulas, 
-- indique si el conjunto es satisfacible.
satisfConj :: [Prop] -> Bool
satisfConj l = error "Funcion a implementar"

-- | insatisfenConj. Función que dada una interpretación y un conjunto de 
-- fórmulas, indique si el conjunto es insatisfacible en el estado dado. 
insatisfenConj :: Estado -> [Prop] -> Bool
insatisfenConj e lp = error "Funcion a implementar"

-- | insatisfConj. Función que dado un conjunto de fórmulas, indique si el 
-- conjunto es insatisfacible.
insatisfConj :: [Prop] -> Bool
insatisfConj l = error "Funcion a implementar"
