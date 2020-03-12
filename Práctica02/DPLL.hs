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

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- Seccion de funciones para la regla de la clausula unitaria

esLiteral :: Clausula -> Bool
esLiteral [x] = True
esLiteral m = False 


unit :: Solucion -> Solucion
unit (m,[]) = (m,[])
unit (m,x:xs)
        | esLiteral x = (m++x,xs)
        | otherwise = (mod,x:form)
            where (mod,form) = unit(m,xs)

--  Seccion de funciones para la regla de eliminacion


elim :: Solucion -> Solucion
elim ([],m) = ([],m)
elim(m,[]) = (m,[])
elim (m, f) = (m, [c| c<-f, [y| y<-c, elem y m]==[]])


-- Seccion de funciones para la regla de reduccion

redC :: Modelo->Clausula->Clausula
redC m c = [clau | clau<-c, not(negElem clau m)]

red :: Solucion -> Solucion
red s@(m, []) = s
red (m, c:f) = (m, (redC m c):reducido)
        where (_, reducido) = red (m, f)



negElem:: Literal->Modelo->Bool
negElem (Neg l) m =  elem l m
negElem _ _ = False 

-- Seccion de funciones para la regla de separacion

 
literales:: Formula -> [Literal]
literales[] = []
literales [y] = [x | x<-y]
literales[y:ys] = [x| x <- literales [ys],not(elem x (literales [[y]]) ) ]

split :: Solucion -> [Solucion]
split s@(m,f) = case [x|x<-literales f,not(elem x m),not(elem (Neg x) m)] of
        []->[s]
        (x:xs)->[(x:m,f),(Neg(x):m,f)]  


-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict(m,[[]]) = True
conflict(m,f) = False

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m,[]) = True
success(m,f) = False

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

