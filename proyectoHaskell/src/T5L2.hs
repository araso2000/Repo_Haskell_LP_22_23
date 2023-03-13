module T5L2 where

import Data.List
import Data.Char

--Tema 5 - Listado 2 - Ejercicio 1
todosIguales :: Eq a => [a] -> Bool
todosIguales lista = if length([y | y <- lista, y == (head lista)]) == length lista then True else False

--Tema 5 - Listado 2 - Ejercicio 2
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto [] _ = True
subconjunto (x:xs) lista = if elem x lista then subconjunto xs lista else False

--Tema 5 - Listado 2 - Ejercicio 3
data Natural = Cero | Suc Natural

--Apartado A:
sumaNaturales :: Natural -> Natural -> Natural
sumaNaturales Cero n = n
sumaNaturales (Suc m) n = Suc (sumaNaturales m n)

--Apartado B:
restaNaturales :: Natural -> Natural -> Natural
restaNaturales Cero _ = Cero
restaNaturales n Cero = n
restaNaturales (Suc m) (Suc n) = restaNaturales m n

--Apartado C:
productoNaturales :: Natural -> Natural -> Natural
productoNaturales Cero _ = Cero
productoNaturales (Suc Cero) n = n
productoNaturales (Suc m) n = sumaNaturales n (productoNaturales m n)

--Tema 5 - Listado 2 - Ejercicio 4
data Expr = Valor Integer
 |Expr :+: Expr
 |Expr :-: Expr
 |Expr :*: Expr deriving Show 

--Apartado A:
valorExpr :: Expr -> Integer
valorExpr (Valor n) = n
valorExpr (e1 :+: e2) = valorExpr e1 + valorExpr e2
valorExpr (e1 :-: e2) = valorExpr e1 - valorExpr e2
valorExpr (e1 :*: e2) = valorExpr e1 * valorExpr e2

--Apartado B:
constantesExpr :: Expr -> [Integer]
constantesExpr (Valor n) = [n]
constantesExpr (e1 :+: e2) = constantesExpr e1 ++ constantesExpr e2
constantesExpr (e1 :-: e2) = constantesExpr e1 ++ constantesExpr e2
constantesExpr (e1 :*: e2) = constantesExpr e1 ++ constantesExpr e2

--Apartado C:
operadoresExpr :: Expr -> [Char]
operadoresExpr (Valor n) = []
operadoresExpr (e1 :+: e2) = operadoresExpr e1 ++ operadoresExpr e2 ++ "+"
operadoresExpr (e1 :-: e2) = operadoresExpr e1 ++ operadoresExpr e2 ++ "-"
operadoresExpr (e1 :*: e2) = operadoresExpr e1 ++ operadoresExpr e2 ++ "*"

--Tema 5 - Listado 2 - Ejercicio 5
type Nombre = String
type Edad = Integer
data Persona = P Nombre Edad deriving (Show, Eq)

qs :: Ord a => [a] -> [a]
qs [] = []
qs (p:xs) = qs [x|x<-xs,x<p] ++ [p] ++ qs [x|x<-xs,x>=p]

--Tema 5 - Listado 2 - Ejercicio 6
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

perteneceArbol :: Ord a => a -> Arbol a -> Bool
perteneceArbol _ AV = False
perteneceArbol n (Rama i r d) = if n == r then True else if n < r then perteneceArbol n i else perteneceArbol n d

--Tema 5 - Listado 2 - Ejercicio 7
pertenece :: Ord a => a -> Arbol a -> Bool
pertenece _ AV = False
pertenece x (Rama izq y der)
  | x == y = True
  | x < y  = pertenece x izq
  | x > y  = pertenece x der

--Tema 5 - Listado 2 - Ejercicio 8
eliminarDuplicados :: Ord a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = if elem x xs then eliminarDuplicados xs else x : eliminarDuplicados xs

--Tema 5 - Listado 2 - Ejercicio 9
listaTienePropiedad :: (a -> Bool) -> [a] -> Bool
listaTienePropiedad _ [] = False
listaTienePropiedad f (x:xs) = if f x then True else listaTienePropiedad f xs