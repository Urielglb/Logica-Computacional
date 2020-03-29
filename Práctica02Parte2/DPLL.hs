{-
- Logica Conmputacional 2020-2 
- Practica02 Parte 1, Implementación del algoritmo dpll.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module DPLL where

import LProp
import Data.List
import Control.Exception

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

data MyException = FAIL 
     deriving Show
instance Exception MyException
 


obtenSolucion:: [Solucion]-> Solucion
obtenSolucion [] =  throw FAIL
obtenSolucion (x:l)
  | success x = x
  | otherwise = obtenSolucion l

noContiene:: Literal -> Modelo -> Bool
noContiene l [x] 
        | l == x = False
        | otherwise = True
noContiene l (x:xs)
        | l==x = False 
        | otherwise = noContiene l xs


-- Seccion de funciones para la regla de la clausula unitaria
esLiteral :: Clausula -> Bool
esLiteral [x] = True
esLiteral m = False

unit :: Solucion -> Solucion
unit s@(m,f) = case[head l | l <- f, esLiteral l] of
        []->s
        v -> case [b | b<-v, noContiene b m ] of
                [] -> s
                w -> obtenSolucion([dpll((m `union` [a]), f \\ [[a]])| a <- w])

--  Seccion de funciones para la regla de eliminacion

elimLit :: Literal-> Formula -> Formula
elimLit lit form = [c| c<-form, not (elem lit c)]

--elimLit p [[p, q],[q, r]]
--      c = [p, q] 
--      not( elem p [p, q] ) = not(True) = False

--     c = [q, r]
--     not elem p [q, r] = not (False) = True

-- elimLit p [[p, q],[q, r]] = [[q,r]]





elim :: Solucion -> Solucion
elim ([],m) = ([],m)
elim (m, f) = obtenSolucion([dpll (m,  elimLit lit f) | lit<-m])
--elim(m,[]) = (m,[])
--elim (m, f) = (m, [c| c<-f, [l| l<-c, elem l m]==[]])
--             clausula  formula  literal

-- Seccion de funciones para la regla de reduccion

redC :: Modelo->Clausula->Clausula
redC m c = [clau | clau<-c, not(negElem clau m)]

negElem:: Literal->Modelo->Bool
negElem (Neg l) m =  elem l m
negElem _ _ = False

red :: Solucion -> Solucion
red s@(m, []) = s
red (m, c:f) = (m, (redC m c):reducido)
        where (_, reducido) = red (m, f)

-- Seccion de funciones para la regla de separacion
lc :: Literal -> Literal
lc (Neg x) = x
lc x = Neg x

literales:: Formula -> [Literal]
literales[] = []
literales (y:ys) = union y (literales ys)


split :: Solucion -> Solucion
split s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict(m,[[]]) = True
conflict(m,f) = False

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m,[]) = True
success(m,f) = False

-- Seccion de las funciones principales de DPLL

dpllsearch :: Solucion -> Solucion
dpllsearch (m,f) = error "Funcion a implementar"

dpll :: Solucion -> Solucion
dpll (m,f) = error "Funcion a implementar"

main :: Solucion -> Solucion
main s = error "Funcion a implementar"

-- Ejemplos

bueno = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exe1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exe2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exe3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exe4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exe5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exe6 = [[V "p"], [Neg (V "p")]]        

ejemplo1 = main ([], exe1)
ejemplo2 = main ([], exe2)
ejemplo3 = main ([], exe3)
ejemplo4 = main ([], exe4)
ejemplo5 = main ([], exe5)
ejemplo6 = main ([], bueno)   
ejemplo7 = main ([], exe6)

p = V "p"
q = V "q"
r = V "r"
s = V "s"
t = V "t"