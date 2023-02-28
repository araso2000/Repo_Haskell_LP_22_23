module T3L2 where
import Data.Char

--Tema 3 - Listado 2 - Ejercicio 1
contarApariciones :: String -> Char -> Int
contarApariciones "" _ = 0
contarApariciones texto letra = length([y | y <- texto, y == letra])

--Tema 3 - Listado 2 - Ejercicio 2
manipula3Tuplas :: ((String,Int),(String,Int),(String,Int)) -> (String, String, String)
manipula3Tuplas ((a,_), (b,_), (c,_)) = (a, b, c)

--Tema 3 - Listado 2 - Ejercicio 3
sumaMenor10 :: [Int] -> Bool
sumaMenor10 (a:b:c:d:xs) = if (a+b+c+d) < 10 then True else False

--Tema 3 - Listado 2 - Ejercicio 4
puntoCardinal :: Char -> String
puntoCardinal 'N' = "Norte"
puntoCardinal 'S' = "Sur"
puntoCardinal 'E' = "Este"
puntoCardinal 'O' = "Oeste"
puntoCardinal _   = "El caracter introducido no pertenece a un punto cardinal" 

--Tema 3 - Listado 2 - Ejercicio 5
todosIguales :: Int -> [Int] -> Bool
todosIguales num lista = if (length([y | y <- lista, y == num]) == length(lista)) && length(lista) > 0 then True else False

--Tema 3 - Listado 2 - Ejercicio 6
mensajeFrase :: String -> String
mensajeFrase texto = "La primera letra de la frase Ejercicios del Tema 3 es " ++ show(head(texto)) ++ " y la ultima letra es " ++ show(last(texto)) 

--Tema 3 - Listado 2 - Ejercicio 7
clasificarValorEntrada :: Int -> String
clasificarValorEntrada x
                        | x >= 10 && x <= 20 = "El valor de entrada es mayor o igual a 10 y menor o igual a 20"
                        | x < 10 = "El valor de entrada es menor que 10" 
                        | x > 20 = "El valor de entrada es mayor que 20" 

--Tema 3 - Listado 2 - Ejercicio 8
listaDivisores :: Int -> [Int]
listaDivisores x = [y | y <- [1..x `div` 2], (x `mod` y) == 0]

amigos :: (Int, Int) -> Bool
amigos (x,y) = sum (listaDivisores x) == y && sum (listaDivisores y) == x

--Tema 3 - Listado 2 - Ejercicio 9
contarConsonantes :: String -> Int
contarConsonantes texto = length([y | y <- texto, y `elem` "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"])

--Tema 3 - Listado 2 - Ejercicio 10
esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..intSqrt n]
    where intSqrt = floor . sqrt . fromIntegral

listaMersenne :: Int -> [Int]
listaMersenne n = take n [2^p - 1 | p <- [2..], esPrimo p]

--Tema 3 - Listado 2 - Ejercicio 11
listasIguales :: [Int] -> [Int] -> Bool
listasIguales xs ys = (length xs == length ys) && and [x == y | (x,y) <- zip xs ys]

--Tema 3 - Listado 2 - Ejercicio 12
head' :: [Int] -> Int
head' (x:xs) = x

--Tema 3 - Listado 2 - Ejercicio 13
tail' :: [Int] -> [Int]
tail' (x:xs) = xs

--Tema 3 - Listado 2 - Ejercicio 14
mayorDivision :: Int -> Int -> Int
mayorDivision x y = maximum[(div x y) , (mod x y)]

--Tema 3 - Listado 2 - Ejercicio 15
sumaTipos :: Int -> Float -> Float
sumaTipos x y = (fromIntegral x) + y

--Tema 3 - Listado 2 - Ejercicio 16
cuadruple :: Int -> Int
cuadruple x = 4*x

--Tema 3 - Listado 2 - Ejercicio 17
calificacion :: Float -> String
calificacion x 
                | x < 5 = "Suspenso"
                | x >= 5 && x < 6 = "Aprobado"
                | x >= 6 && x < 8 = "Notable"
                | x >= 8 && x < 10 = "Sobresaliente"
                | x == 10 = "Matricula de honor"

--Tema 3 - Listado 2 - Ejercicio 18
cuadrado :: [Int] -> [Int]
cuadrado lista = [y*y | y <- lista, even y]

--Tema 3 - Listado 2 - Ejercicio 19
posicionEnLista :: [Int] -> [(Int, Int)]
posicionEnLista lista = [(x,y) | y <- [0..(length lista)-1] , let x = lista !! y]

--Tema 3 - Listado 2 - Ejercicio 20
longitudLista :: [Int] -> Int
longitudLista lista = sum [1 | _ <- lista]

--Tema 3 - Listado 2 - Ejercicio 21
contiene :: Int -> [Int] -> Bool
contiene x lista = length([y | y <- lista, y == x]) > 0

--Tema 3 - Listado 2 - Ejercicio 22
primeros :: [(Char, Int)] -> String
primeros lista = [y | (y, _) <- lista]

--Tema 3 - Listado 2 - Ejercicio 23
primerosPares :: [(Char, Int)] -> String
primerosPares lista = [y | (y, x) <- lista, even x]

--Tema 3 - Listado 2 - Ejercicio 24
partir :: Int -> [Int] -> ([Int],[Int])
partir n lista = (take n lista, drop n lista)

--Tema 3 - Listado 2 - Ejercicio 25
insertar :: [Int] -> Int -> Int -> [Int]
insertar lista valor pos = take pos lista ++ [valor] ++ drop pos lista

--Tema 3 - Listado 2 - Ejercicio 26 ???
--codifica :: [Int] -> String
--codifica lista = [y | ]

--Tema 3 - Listado 2 - Ejercicio 27 ???
listaPotencias :: [Int] -> [Int]
listaPotencias lista = [y^x | y <- lista , x <- [((length lista) - 1)..0]]

--Tema 3 - Listado 2 - Ejercicio 28 ???
--esPerfecto :: Int -> Bool
--esPerfecto x = 
    
--listaPerfectos :: Int
--listaPerfectos n = take n [y | y <- [1..],]

--Tema 3 - Listado 2 - Ejercicio 29 ???