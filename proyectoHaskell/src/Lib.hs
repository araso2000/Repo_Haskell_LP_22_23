module Lib where

celsius :: Int -> Int
celsius x = (x - 32) * 5 `div` 9

doble :: Int -> Int
doble x = 2*x

suma :: Int -> Int -> Int
suma x y = x + y

composicion :: Int->Int->Int
composicion x = (suma x).doble
--Primero se ejecutaria DOBLE con el numero 2 y luego se sumaria ese doble (que seria 4) a 1, es decir, 5

compCuatro :: Int -> Int -> Int
compCuatro x = (suma x).negate.(suma x).doble

divEntera :: (Int, Int) -> (Int, Int)
divEntera (m,n) = (m `div` n, m `rem` n)

--Definir una función que dados dos intervalos de numeros enteros diga si hay intersección entre ellos
interseccion :: (Int, Int) -> (Int,Int) -> Bool
interseccion (m,n) (x,y) = not (n < x)

--Definir una funcion que dados tres numeros enteros devuelva el mayor de ellos. Definir la funcion con guardas
mayorDe3 :: Int -> Int -> Int -> Int
mayorDe3 x y z
            | x > y && y > z = x
            | y > z && z > x = x
            | x > y && y > z = x


--Tema 3 - Listado 0 - Ejercicio 1
componer :: Int -> Int -> Int
componer x y = x+y

--Tema 3 - Listado 0 - Ejercicio 2
sucesor :: Int -> Int
sucesor x = x + 1

--Tema 3 - Listado 0 - Ejercicio 3
cuadruple :: Int -> Int
cuadruple x = 2 * doble x

--Tema 3 - Listado 1 - Ejercicio 1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z
                    | ((x < y) && (y < z)) = True
                    | otherwise False
                
--Tema 3 - Listado 1 - Ejercicio 2
ordenarTupla :: (Int,Int,Int) -> (Int,Int,Int)
ordenarTupla (x, y, z) = 