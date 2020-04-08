{-
- Logica Conmputacional 2020-2 
- Sustitucion para LPO.
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}
module LPO where

--Modulo de listas brNombreados por el lenguaje
import Data.List

--Tipo que representa el nombre.
type Nombre = String

--Tipo que representa un término.
data Term = V Nombre | F Nombre [Term] deriving(Show,Eq,Ord)

--Tipo que representa una fórmula de LPO.
data Form = TrueF
          | FalseF
          | Pr Nombre [Term]
          | Eq Term Term
          | Neg Form
          | Conj Form Form
          | Disy Form Form
          | Imp Form Form
          | Equi Form Form
          | All Nombre Form
          | Ex Nombre Form deriving(Show,Eq,Ord)

-- | Subst. Tipo que representa una sustitución de variables en términos.
type Subst = [(Nombre,Term)]          

-- | varT. Función que devuelve una lista con todos los índices de variables que
-- figuran en t. (De regalo, les va a servir para apsubF en los casos de All y Ex)
varT :: Term -> [Nombre]
varT (V x) = [x]
varT (F f l) = foldl1 union [varT t | t <- l]

-- | verifSus. Función que verifica una sustitución.
verifSus :: Subst -> Bool
verifSus s = error "funcion a implementar."

-- | apsubT. Función que aplica una sustitución de variables de variables en términos
-- en un término dado.
apsubT :: Term -> Subst -> Term
apsubT t s = error "funcion a implementar."

-- | apsubfuncion que devuelve la sustitucion de terminos en una formula 
apsubF :: Form -> Subst -> Form
apsubF f s = error "funcion a implementar."