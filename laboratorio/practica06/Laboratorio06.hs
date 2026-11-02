module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = auxiliar base exponente 1
  where
    auxiliar base 0 acumulador = acumulador
    auxiliar base exponente acumulador =
      auxiliar base (exponente - 1) (base * acumulador)


potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS base 0 k = k 1
potenciaCPS base exponente k =
  potenciaCPS base (exponente - 1) (\resultado -> k (base * resultado))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = auxiliar xs 0
  where
    auxiliar [] acumulador = acumulador
    auxiliar (_ : xs) acumulador = auxiliar xs (acumulador + 1) 

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (_ : xs) k =
  longitudCPS xs (\resultado -> k (1 + resultado)) -- lambda --

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = auxiliar xs []
  where
    auxiliar [] acumulador = acumulador
    auxiliar (x : xs) acumulador = auxiliar xs (x : acumulador)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k =
  reversaCPS xs (\resultado -> k (resultado ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xs = auxiliar xs []
  where
    auxiliar [] acumulador = acumulador
    auxiliar (xs : xss) acumulador = auxiliar xss (acumulador ++ xs) 

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k =
  concatenaCPS xss (\resultado -> k (xs ++ resultado))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola [] ys = acumulador ys 
intercalaCola xs [] = acumulador xs 
  where
    auxiliar [] 

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS = undefined
