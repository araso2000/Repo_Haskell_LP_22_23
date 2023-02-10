module Lib where
import Data.Char

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

--Definir una funcion que dados tres numeros enteros devuelva el mayor de ellos. Se debe utilizar en la definicion de 
--esta funcion otra funcion que calcule el maximo de dos numeros enteros
maximoDos :: Int -> Int -> Int
maximoDos x y = if x >= y then x else y

mayorDeTres :: Int -> Int -> Int -> Int
mayorDeTres x y = maximoDos x.maximoDos y

mayorDeTres' :: Int -> Int -> Int -> Int
mayorDeTres' x = maximoDos.maximoDos x


--Definir una funcion que dados dos numeros devuelva una lista con el resultado de sumar, restar y multiplicar
--ambos numeros
listaOperaciones :: Int -> Int -> [Int]
listaOperaciones x y = [x+y, x-y, x*y]

--Definir una funcion que dadas dos cadenas de caracteres, si ambos tienen una longitud menor o igual a tres devolvera
--su union, en otro caso devolvera una cadena vacia
longCadenas :: [Char] -> [Char] -> [Char]
longCadenas x y = if length x <= 3 && length y <=3 then x ++ y else [ ]
--Tambien se puede hacer con String, que soy bobo

--Definir una funcion que dada una frase devuelva una cadena formada unicamente por los caracteres en mayusculas
listaMayus :: String -> String
listaMayus x = [y | y <- x, isUpper y]

abecedario :: String
abecedario = [x | x <- "El perro de San Roque no tiene rabo", isUpper x]

--(cabeza:resto) esto es una lista
--[cabeza:resto] esto es una lista de listas
--[x,y] lista limitada segun los elementos que indiquemos como variables


--Tema 3 - Listado 0 - Ejercicio 1
componer :: Int -> Int -> Int
componer x y = x+y

--Tema 3 - Listado 0 - Ejercicio 2
sucesor :: Int -> Int
sucesor x = x + 1

--Tema 3 - Listado 0 - Ejercicio 3
cuadruple :: Int -> Int
cuadruple x = 2 * doble x
