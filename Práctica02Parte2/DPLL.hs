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
noContiene l [] = False
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

allUnit::Solucion ->[Solucion]
allUnit s@(m,f) = case [head l | l <- f, esLiteral l] of
        []->[s]
        v -> case [b | b<-v, not (negElem b m)] of
                [] -> [s]
                w -> [((m `union` [a]), f \\ [[a]])| a <- w]

unit :: Solucion -> Solucion
unit s@(m,f) = case [head l | l <- f, esLiteral l] of
        []->s
        v -> case [b | b<-v, not (negElem b m)] of
                [] -> s
                w -> obtenSolucion([dpll((m `union` [a]), f \\ [[a]])| a <- w])

--  Seccion de funciones para la regla de eliminacion

elimLit :: Literal-> Formula -> Formula
elimLit lit form = [c | c<-form, not (elem lit c)]

allElim :: Solucion -> [Solucion]
allElim s@(m,[]) = [s]
allElim (m, f) =[ (m,  elimLit lit f) | lit<-m]


elim :: Solucion -> Solucion
elim s@(m,[]) = s
elim (m, f) = obtenSolucion([dpll (m,  elimLit lit f) | lit<-m])


-- Seccion de funciones para la regla de reduccion

redC :: Modelo->Clausula->Clausula
redC m c = [clau | clau<-c, not(negElem clau m)]

redLit:: Literal->Formula->Formula
redLit l f = [[lit | lit<-c, not (negElem l [lit])] | c<-f]

negElem:: Literal->Clausula->Bool
negElem (Neg l) c =  elem l c
negElem l c =  elem (Neg l) c

allRed::Solucion->[Solucion]
allRed s@(m, []) = [s]
allRed (m, f) = [(m,  redLit lit f) | lit<-m]

red :: Solucion -> Solucion
red s@(m, []) = s
red (m, f) = obtenSolucion([dpll (m,  redLit lit f) | lit<-m])

-- Seccion de funciones para la regla de separacion
lc :: Literal -> Literal
lc (Neg x) = x
lc x = Neg x

literales:: Formula -> [Literal]
literales[] = []
literales (y:ys) = union y (literales ys)

quitaNeg:: [Literal]-> [Literal]
quitaNeg [] = []
quitaNeg ((Neg x):xs) = x:(quitaNeg xs)
quitaNeg (x:xs) = x:(quitaNeg xs)

allSplit:: Solucion->[Solucion]
allSplit s@(m,f) = case [l | l<- (nub $ quitaNeg $ literales f), not(elem l m), not(negElem l m)]of
                  []->[s]
                  v -> [(lit:m, f) | lit<-v]++[((Neg lit):m, f) | lit<-v]


split :: Solucion -> Solucion
split s@(m, []) = s
split s@(m,f) = case [l | l<- (nub $ quitaNeg $ literales f), not(elem l m), not(negElem l m)]of
                  []->s
                  v -> obtenSolucion([dpll (lit:m, f) | lit<-v]++[dpll ((Neg lit):m, f) | lit<-v])

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
dpllsearch s@(m,f) = split $ red $ elim $ unit s

dpll :: Solucion -> Solucion
dpll s@(m,f) = case success s of
    True -> s
    False ->  case conflict s of
        True -> s
        False -> dpll (dpllsearch s)

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
