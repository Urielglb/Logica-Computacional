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
import Data.List

-- | Estado. Tipo que representa un estado de variables.
type Estado = [VarP]

-- | i. Implementación de la función de interpretación.
i :: Estado -> Prop -> Bool
i e TTrue = True
i e FFalse = False
i e (V p) =  elem p e
i e (Neg p) = not (i e p)
i e (Conj p q) = and [(i e p), (i e q)]
i e (Disy p q) = or [(i e p), (i e q)]
i e (Imp p q) =  or [iq, and [not ip, not iq]]
  where ip = i e p
        iq = i e q
i e (Equiv p q) = and [i e (Imp p q), i e (Imp q p)]

-- | vars. Función que devuelve el conjunto de variables proposicionales de una
-- fórmula.
vars :: Prop -> [VarP]
vars p = conjuntisa (varsAux (cnf p))

varsAux :: Prop -> [VarP]
varsAux TTrue = []
varsAux FFalse = []
varsAux (V p) = [p]
varsAux (Neg p) = varsAux p
varsAux (Disy p q) = (varsAux p)++(varsAux q)
varsAux (Conj p q) = (varsAux p)++(varsAux q)

-- | estados. Función que devuelve todos los posibles estados de una fórmula.
estados :: Prop -> [Estado]
estados p = subconj $ vars p

-- | modelos. Función que devuelve todos los posibles modelos de una fórmula.
modelos :: Prop -> [Estado]
modelos p = [m | m<-estados p, i m p]

-- | tautologia. Función que indica si una fórmula es una tautología.
tautologia :: Prop -> Bool
tautologia p = (length $ modelos p)==(length $ estados p)

-- | satisfen. Función que determina si una fórmula es satisfacible en un
-- estado dado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = i e p

-- | satisf. Función que determina si una fórmula es satisfacible.
satisf :: Prop -> Bool
satisf p = (modelos p)/=[]

-- | insatisfen. Función que determina si una fórmula es insatisfacible en un
-- estado dado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = not (i e p)

-- | satisf. Función que determina si una fórmula es una contradicción.
contrad :: Prop -> Bool
contrad p = (modelos p)==[]

-- | equiv. Función que determina si dos fórmulas son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = mp1 == mp2
  where mp1 = sort $ modelos p1
        mp2 = sort $ modelos p2

-- | estadosConj. Función que dado un conjunto de formaulas, devuleva el
-- conjunto de todos los posibles estados del conjunto de formulas. la funcion Union2 esta en el modulo LConj
estadosConj :: [Prop] -> [Estado]
estadosConj lp = union2 [e | p<-lp, e<-estados p]

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
