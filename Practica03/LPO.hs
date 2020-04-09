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

auxVerifSus :: (Nombre,Term) -> Bool
auxVerifSus (n,V t) 
    | n == t = False
    | otherwise = True
auxVerifSus (n, F t xs) = and [auxVerifSus (n, x) | x<-xs]

-- | verifSus. Función que verifica una sustitución.
verifSus :: Subst -> Bool
verifSus [x] = auxVerifSus x
verifSus(x:xs) 
    | verifSus [x] == False = False
    | otherwise = verifSus xs 

-- | apsubT. Función que aplica una sustitución de variables de variables en términos
-- en un término dado.
apsubT :: Term -> Subst -> Term
apsubT t [] = t
apsubT (V var) ((n, t):xs)
    | var==n  = t
    | otherwise = apsubT  (V var) xs
apsubT (F fun terms) s = (F fun [ apsubT t s | t<-terms])


-- | apsubfuncion que devuelve la sustitucion de terminos en una formula 
apsubF :: Form -> Subst -> Form
apsubF (TrueF) s = TrueF
apsubF (FalseF) s = FalseF
apsubF (Pr n xs) s = Pr n [apsubT x s | x <-xs]
apsubF (Eq a b) s = Eq (apsubT a s) (apsubT b s)
apsubF (Neg f) s = Neg (apsubF f s)
apsubF (Conj a b) s = Conj (apsubF a s) (apsubF b s)
apsubF (Disy a b) s = Disy (apsubF a s) (apsubF b s) 
apsubF (Imp a b) s = Imp (apsubF a s) (apsubF b s)
apsubF (Equi a b) s= Equi (apsubF a s) (apsubF b s)

--apsubF All Nombre Form
--          | Ex Nombre Form 

