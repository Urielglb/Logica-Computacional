-- | Logica Computacional 2020-02
-- | Practica 1: Introduccion a Haskell
-- | Profesor: Dr. Favio Ezequiel Miranda Perea
-- | Ayudante: Alejandra Krystel Coloapa Díaz
-- | Laboratorio: Pedro Juan Salvador Sánchez Pérez

module Binario where

-- | tipo de dato Binario, es la representacion de todos los numero binarios que empiezan con uno
data Binario = U | Cero Binario | Uno Binario

-- | definicion de la clase Show para el tipo de dato Binario
instance Show Binario where

    show (U) = "1"
    show (Cero b) = show b ++ "0"
    show (Uno b) = show b ++ "1"

-- | sucesor. Regresa el sucesor de un Binario
-- -> ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor (U) = (Cero U)
sucesor (Cero a) = (Uno a)
sucesor (Uno a) = (Cero (sucesor a))

antecesor :: Binario -> Binario
antecesor (Cero U) = U
---        [1,0]   [1]
antecesor (Uno a) = (Cero a)
antecesor (Cero a) = (Uno (antecesor a))

-- | suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma U b2 = sucesor(b2)
suma b1 b2 = sucesor (suma (antecesor b1) b2)


-- | producto. Regresa el producto de 2 numeros Binarios
-- -> ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
producto U b2 = b2
producto b1 b2 = suma (producto (antecesor b1) b2) b2

-- | natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 0 = [1]
natBinLista 1 = [1]
natBinLista n
    | mod n 2 == 0 = ((natBinLista(div n 2)) ++ [0])
    |otherwise = ((natBinLista(div n 2))++[1])


natABin :: Int -> Binario
natABin 1 = U
natABin n
  | mod n 2 == 0 = (Cero (natABin(div n 2)))
  |otherwise = (Uno (natABin(div n 2)))


-- | sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista l1 l2 = suma (lista2Binario (reverse l1)) (lista2Binario (reverse l2))

lista2Binario:: [Int] -> Binario
lista2Binario (i:[]) = U
lista2Binario (1:b) = (Uno (lista2Binario b))
lista2Binario (0:b) = (Cero (lista2Binario b))
