module Clase where

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

--Dada una funcion que dada una lista de caracteres indique si su longitud es menor o igual a tres elemento. No se puede utilizar recursividad, ni funciones de longitud predefinidas
longMenorTres :: String -> Bool
longMenorTres [] = True
longMenorTres [x] = True
longMenorTres [x,y] = True
longMenorTres _ = True

longMenorTres' :: String -> Bool
longMenorTres' (c1:c2:c3:c4:cs) = False
longMenorTres' _ = True

--Definir una funcion que dada una cadena de caracteres indique si comienza o no por mayuscula
primeraMayus :: String -> Bool
primeraMayus (x:xs) = x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

primeraMayus' :: String -> Bool
primeraMayus' (x:_) = isUpper x
primeraMayus' _ = False

--Definir una funcion que dadas dos cadenas de carateres las devuelva ordenadas alfabeticamente segun su primer caracter. Sin recursividad, si una cadena está vacía, irá en primer lugar
ordenarAlfabeto :: String -> String -> (String,String)
ordenarAlfabeto lista [] = ([],lista)
ordenarAlfabeto [] lista = ([], lista)
ordenarAlfabeto c1@(x:xs) c2@(y:ys) = if (toLower x) < (toLower y) then (c1,c2) else (c2,c1)

--Definir una funcion recursiva que dadas una cadena y un caracter, cuente el numero de apariciones del caracter en cadena
countChar :: Char -> String -> Int
countChar _ [] = 0  -- Caso base: la cadena es vacía, no hay apariciones del caracter
countChar c (x:xs)
                    | c == x    = 1 + countChar c xs  -- El primer caracter de la cadena es igual al buscado, sumamos 1 y seguimos buscando en el resto
                    | otherwise = countChar c xs     -- El primer caracter de la cadena es diferente al buscado, seguimos buscando en el resto

--Definir una funcion recursiva que dada una lista de numeros enteros devuelva como resultado la suma de todos ellos
sumaNumeros :: [Int] -> Int
sumaNumeros [] = 0
sumaNumeros (x:xs) = x + sumaNumeros xs

sumaNumerosTail :: [Int] -> Int
sumaNumerosTail lista = sumaTail 0 lista
    where
        sumaTail :: Int -> [Int] -> Int
        sumaTail res [] = res
        sumaTail res (x:xs) = sumaTail (res + x) xs


--Foldr recorre de derecha a izquierda y foldl de izquierda a derecha la lista dada
-- foldr = (w + (x + (y + (z + e) ) ) )
-- foldl = ( ( ( (w + x) + y) + z) + e)

--Definir una funcion que calcule la longitud de una lista de numeros enteros utilizando la funcion de plegado foldr
--funciones anonimas
longListaPlegado :: [Int] -> Int
longListaPlegado lista = foldr (\_ n -> n + 1) 0 lista

--Definir una funcion en haskell que reciba una lista de funciones que se aplican a un segundo argumento de tipo entero
--y retorna una lista de enteros con el resultado de aplicar cada funcion al segundo argumento
aplicarFuncion :: [Int -> Int] -> Int -> [Int]
aplicarFuncion fs x = map (\f -> f x) fs

aplicarFuncion' :: [Int -> Int] -> Int -> [Int]
aplicarFuncion' [] _ = []
aplicarFuncion' (f:fs) n = f n : aplicarFuncion' fs n

aplicarFuncion'' :: [Int -> Int] -> Int -> [Int]
aplicarFuncion'' lista n = [f n | f <- lista]

aplicarFuncion''' :: [Int -> Int] -> Int -> [Int]
aplicarFuncion''' lista n = foldr (\f ac -> [f n] ++ ac) [] lista