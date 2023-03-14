module RepasoParcial where

import Data.List
import Data.Char

--Repaso 1 - Ejercicio 1
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece n xs = foldl(\acu x -> acu || (x == n)) False xs

--Repaso 1 - Ejercicio 2
eliminar :: (Eq a) => a -> [a] -> [a]
eliminar e lista = foldr(\x acu -> if x == e then acu else x : acu) [] lista

--Repaso 1 - Ejercicio 3
productoEscalar :: Num a => [a] -> [a] -> a
productoEscalar v1 v2 = foldr (+) 0 [x*y | (x,y) <- zip v1 v2]

--Repaso 2 - Ejercicio 1
--Apartado A

data Calzado = Bota {marcaB :: String, numeroB :: Int} | Zapatilla {marcaZ :: String, numeroZ :: Int, deporteZs :: String} deriving Eq

--Apartado B

instance Show Calzado where
    show(Bota m n) = "Bota de la marca " ++ m ++ " del numero " ++ show(n)
    show(Zapatilla m n d) = "Zapatilla de la marca " ++ m ++ " del numero " ++ show(n) ++ " para " ++ d

productos :: (Show Calzado) => [Calzado] -> String
productos lista = show(lista)

bota1 :: Calzado
bota1 = Bota "Fosco" 38

zapatilla2 :: Calzado
zapatilla2 = Zapatilla "Adidas" 40 "Running"

lista :: [Calzado]
lista = [bota1,zapatilla2]

--Apartado C

instance Ord Calzado where
  compare (Bota marca1 _) (Bota marca2 _) = compare marca1 marca2
  compare (Zapatilla marca1 _ _) (Zapatilla marca2 _ _) = compare marca1 marca2
  compare (Bota marca1 _) (Zapatilla marca2 _ _) = compare marca1 marca2
  compare (Zapatilla marca1 _ _) (Bota marca2 _) = compare marca1 marca2    

order :: Ord a => [a] -> [a]
order [] = []
order (x:xs) =
    order menores ++ [x] ++ order mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y > x]

--Repaso 2 - Ejercicio 2
--Apartado A 
data Version = Version {major :: Int, minor :: Int}

data Libreria = Libreria {nombre :: String, version :: Version}

--Apartado B
instance Eq Version where
    (Version ma1 mi1) == (Version ma2 mi2) = ma1==ma2 && mi1==mi2

instance Eq Libreria where
    (Libreria n1 v1) == (Libreria n2 v2) = n1==n2 && v1==v2

instance Ord Version where
  (Version ma1 mi1) <= (Version ma2 mi2) = if ma1 == ma2 then mi1 <= mi2 else ma1 <= ma2
  (Version ma1 mi1) <  (Version ma2 mi2) = if ma1 == ma2 then mi1 <  mi2 else ma1 <  ma2
  (Version ma1 mi1) >= (Version ma2 mi2) = if ma1 == ma2 then mi1 >= mi2 else ma1 >= ma2
  (Version ma1 mi1) >  (Version ma2 mi2) = if ma1 == ma2 then mi1 >  mi2 else ma1 >  ma2


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) =
    ordenar menores ++ [x] ++ ordenar mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y > x] 

--Apartado C    
class Compatible a where
  isCompatible :: a -> a -> Bool

instance Compatible Libreria where
  isCompatible (Libreria n1 (Version ma1 _)) (Libreria n2 (Version ma2 _)) = n1==n2 && ma1 == ma2


--Apartado D
instance Show Libreria where
    show(Libreria n (Version ma mi)) = n ++ " " ++ show(ma) ++ "." ++ show(mi)

compatibles :: (Compatible Libreria) => Libreria -> [Libreria] -> [Libreria]
compatibles _ [] = []
compatibles n (x:xs) = if isCompatible n x then x : compatibles n xs else compatibles n xs

lib1 :: Libreria
lib1 = Libreria "docker" (Version 2 1)

lib2 :: Libreria
lib2 = Libreria "docker" (Version 2 3)

lib3 :: Libreria
lib3 = Libreria "docker" (Version 2 5)

lib4 :: Libreria
lib4 = Libreria "docker" (Version 3 2)

lib5 :: Libreria
lib5 = Libreria "docker" (Version 5 1)

lib6 :: Libreria
lib6 = Libreria "mtt" (Version 2 3)

listaLib :: [Libreria]
listaLib = [lib1,lib2,lib3,lib4,lib5,lib6]


--Repaso 3 - Ejercicio 1
--APRENDER DE MEMORIA!!!
data Arbol a = AV | Rama (Arbol a) a (Arbol a)

insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x AV = Rama AV x AV  -- Caso base: si el árbol es vacío, creamos un nuevo nodo con el valor x
insertar x (Rama izq y der)
  | x < y     = Rama (insertar x izq) y der  -- Si x es menor que la raíz, insertamos en el subárbol izquierdo
  | otherwise = Rama izq y (insertar x der)  -- Si x es mayor o igual que la raíz, insertamos en el subárbol derecho

--Repaso 3 - Ejercicio 2
dobleTupla :: String -> (String, String)
dobleTupla palabra = (vocales,(filter (\c -> notElem c vocales) palabra)) 
    where vocales = foldr(\x acu -> if x `elem` "AEIOUaeiou" then x : acu else acu) "" palabra

--Repaso 3 - Ejercicio 3
separarPorPosicion :: [a] -> ([a], [a])
separarPorPosicion [] = ([], [])    -- caso base: lista vacía
separarPorPosicion [x] = ([x], []) -- caso base: lista con un solo elemento
separarPorPosicion (x:y:xs) =      -- caso recursivo: lista con al menos dos elementos
  let (pares, impares) = separarPorPosicion xs
  in (x:pares, y:impares)


--Examen Marzo 2022 - Ejercicio 2
tokenizar :: String -> [String] 
tokenizar frase = tokenizarAux frase [] [] 
 
tokenizarAux :: String -> String -> [String] -> [String] 
tokenizarAux [] token lista = lista ++ [token] 
tokenizarAux (c:cs) token lista = if c /= ' ' then  
                    tokenizarAux cs (token ++ [c]) lista 
                                    else tokenizarAux cs [] (lista++ [token]) 
 
palabraMasLarga :: String -> String 
palabraMasLarga frase = palabraMasLarga' (tokenizar frase) 
 
palabraMasLarga' :: [String] -> String 
palabraMasLarga' lista = palabraMasLargaAux lista [] 
 
palabraMasLargaAux :: [String] -> String -> String 
palabraMasLargaAux [] r = r 
palabraMasLargaAux (x:xs) r = if length x > length r then palabraMasLargaAux xs x 
                                else palabraMasLargaAux xs r


--Examen Marzo 2022 - Ejercicio 3
parejaPosicionCoincidente :: [(Int,a)] -> [a] 
parejaPosicionCoincidente lista = parejas listaPares where  
                                listaPares = [(x,y) | (x, y) <- zip lista [1..]]  
 
parejas :: [((Int,a),Int)] -> [a] 
parejas lista = foldr (\((n,p),pos) ac -> if n==pos then [p]++ac else ac) [] lista

--Examen Marzo 2022 - Ejercicio 4
data List a = Vacia | Cons a (List a)

suma :: (Num a) => List a -> List a -> List a 
suma Vacia Vacia = Vacia 
suma Vacia (Cons n lista) = (Cons n lista) 
suma (Cons n lista) Vacia = (Cons n lista) 
suma (Cons x lis1) (Cons y lis2) = Cons (x+y) (suma lis1 lis2) 

--Examen Marzo 2022 - Ejercicio 5
--Apartado A
data Ubicacion = Ubi {ciudad :: String, pais :: String}

data Capacidad = Cap {individual :: Int, doble :: Int, triple :: Int, suite :: Int}

data Hotel = Hot {nom :: String, estrellas :: Int, ubicacion :: Ubicacion, capacidad :: Capacidad}

--Apartado B

instance Eq Hotel where
  (Hot _ _ _ (Cap i1 d1 _ _)) == (Hot _ _ _ (Cap i2 d2 _ _)) = i1==i2 && d1==d2

instance Ord Hotel where
  (Hot _ _ _ (Cap i1 d1 _ _)) <= (Hot _ _ _ (Cap i2 d2 _ _)) = i1<=i2 || d1<=d2
  (Hot _ _ _ (Cap i1 d1 _ _)) > (Hot _ _ _ (Cap i2 d2 _ _)) = i1>i2 || d1>d2

orderHot :: Ord a => [a] -> [a]
orderHot [] = []
orderHot (x:xs) = orderHot menores ++ [x] ++ orderHot mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y > x] 

--Apartado C
data CasaRural = CR {nomb :: String, espigas :: Int, direccion :: String}

class Impuesto a where
  calcularImpuesto :: a -> Float

instance Impuesto Hotel where
  calcularImpuesto (Hot _ e1 _ (Cap i1 d1 t1 s1)) = if (i1+d1+t1+s1) >= 30 && e1 >= 3 then 345.5 else if (i1+d1+t1+s1) >= 30 && e1 < 3 then 225.5 else 150.5

instance Impuesto CasaRural where 
  calcularImpuesto (CR _ e1 _) = if e1 > 3 then 250.5 else 180.5

--Examen Junio 2022 - Ejercicio 4
type Day = Int
type Month = Int
type Year = Int
data Date = D Day Month Year

data Noticia = RS {texto :: String , fecha :: Date} | Online {titular :: String, texto :: String, fecha :: Date}

data Gestor = GS {listado :: [Noticia]}

instance Show Date where
  show (D d m y) = show d ++ "/" ++ show m ++ "/" ++ show y

instance Show Noticia where
  show (RS c d) = "Contenido: " ++ c ++ " para publicar en red social en la fecha "++ show d
  show (Online h c d) = "Titular: " ++ h ++" y contenido: " ++ c ++" para publicar en periodico en la fecha "++ show d

instance Show Gestor where
  show (GS []) = ""
  show (GS (x:xs)) = show x ++ "\n" ++ show (GS xs) 

n1 :: Noticia 
n1 = RS "nnnnnn1" (D 15 06 2022) 
 
n2 :: Noticia 
n2 = RS "nnnnnn2" (D 16 06 2022) 
 
n3 :: Noticia 
n3 = Online "ttttttt3" "nnnnnnnn3" (D 15 06 2022)

noticias :: [Noticia]
noticias = [n1,n2,n3]

listadoNoticias :: Gestor
listadoNoticias = (GS noticias)

coleccion :: Gestor -> String
coleccion (GS []) = ""
coleccion (GS (x:xs)) = show(x) ++ coleccion (GS xs)

instance Eq Date where
  (D d1 m1 y1) == (D d2 m2 y2) = (d1 == d2) && (m1 == m2) && (y1 == y2)

instance Ord Date where
  compare (D d1 m1 y1) (D d2 m2 y2) = if ((compare y1 y2) == EQ) then
    (if ((compare m1 m2) == EQ) then compare d1 d2 else compare m1 m2) else compare y1 y2

instance Eq Noticia where 
    (RS _ d1) == (RS _ d2) = d1 == d2 
    (Online _ _ d1) == (Online _ _ d2) = d1 == d2 
    (RS _ d1) == (Online _ _ d2) = d1 == d2 
    (Online _ _ d1) == (RS _ d2) = d1 == d2 
 
instance Ord Noticia where 
    compare (RS _ d1) (RS _ d2) = compare d1 d2 
    compare (Online _ _ d1) (Online _ _ d2) = compare d1 d2 
    compare (RS _ d1) (Online _ _ d2) = compare d1 d2 
    compare (Online _ _ d1) (RS _ d2) = compare d1 d2

inserta :: Ord a => a -> [a] -> [a] 
inserta e [] = [e] 
inserta e (x:xs) 
    | e <= x = e:x:xs 
    | otherwise = x : inserta e xs

--T3-L0-Eje1
componer :: (Int, Int) -> Int
componer (a,b) = if cociente >= resto then cociente else resto where
    cociente = a `div` b
    resto = a `mod` b

--T3-L0-Eje2
sucesor :: Int -> Int
sucesor x = x+1

--T3-L0-Eje3
cuadruple :: Int -> Int
cuadruple x = x*4

--T3-L1-Eje1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z = if (x <= y) && (y <= z) then True else False

--T3-L1-Eje2
ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenarTupla (x, y, z)
  | x <= y && y <= z = (x, y, z)
  | x <= z && z <= y = (x, z, y)
  | y <= x && x <= z = (y, x, z)
  | y <= z && z <= x = (y, z, x)
  | z <= x && x <= y = (z, x, y)
  | otherwise = (z, y, x)

--T3-L1-Eje3
descomponerReal :: Double -> (Int, Double)
descomponerReal num = (parteEntera, parteFraccionaria)
  where
    parteEntera = truncate num
    parteFraccionaria = abs (num - fromIntegral parteEntera)

--T3-L1-Eje4
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], (x `mod` y) == 0]

--T3-L1-Eje5
esDigito :: Char -> Bool
esDigito x
  | ord x >= 0 && ord x <= 9 = True
  | otherwise = False

--T3-L1-Eje6
esPrimo :: Int -> Bool
esPrimo x = if length(divisores x) <= 2 then True else False

--T3-L1-Eje7
listaPrimosImpares :: [Int] -> [Int]
listaPrimosImpares lista = [y | y <- lista, (not (even y)) && (esPrimo y)]

--T3-L1-Eje8
primosMenorIgual :: Int -> [Int]
primosMenorIgual num = [y | y <- [1..num], esPrimo y]

--T3-L1-Eje9
codificacionTuplas :: [(Char,Char)] -> String
codificacionTuplas lista = [a | (a,b) <- lista, b `elem` "aeiouAEIOU"]

--T3-L1-Eje10
filtrarTuplas :: [(Int,Int)] -> Int -> [(Int,Int)]
filtrarTuplas lista n = [(a,b)| (a,b) <- lista, not (even a) && (a > n)]

--T3-L1-Eje11
cuantasPitagoricas :: [(Int,Int,Int)] -> Int
cuantasPitagoricas lista = length([(x,y,z) | (x,y,z) <- lista, ((x*x) + (y*y)) == (z*z)])

--T3-L1-Eje12
esMayuscula :: Char -> Bool
esMayuscula letra = if letra `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then True else False

--T3-L1-Eje13
mayusculasMinusculas :: String -> String
mayusculasMinusculas cadena = [if (esMayuscula y) then (toLower y) else (toUpper y) | y <- cadena]

--T3-L1-Eje14
listaASCII :: String -> [Int]
listaASCII texto = [ord y | y <- texto]

--T3-L1-Eje15
mensajeLista :: [Int] -> String
mensajeLista lista = "Primer elemento: " ++ show(head(lista)) ++ ", longitud: " ++ show(length(lista))

--T3-L1-Eje16
contarMayusculas :: String -> Int
contarMayusculas texto = length([y | y <- texto, esMayuscula y])

--T3-L2-Eje1
contarApariciones :: String -> Char -> Int
contarApariciones texto letra = length([y | y <- texto, y == letra])

--T3-L2-Eje2
manipula3Tuplas :: ((String,Int),(String,Int),(String,Int)) -> (String,String,String)
manipula3Tuplas ((a,_),(b,_),(c,_)) = (a,b,c)

--T3-L2-Eje3
sumaMenor10 :: [Int] -> Bool
sumaMenor10 (a:b:c:d:xs) = if (a+b+c+d) < 10 then True else False

--T3-L2-Eje4
puntoCardinal :: Char -> String
puntoCardinal 'N' = "Norte"
puntoCardinal 'S' = "Sur"
puntoCardinal 'E' = "Este"
puntoCardinal 'O' = "Oeste"
puntoCardinal otherwise = "El caracter introducido no pertenece a un punto cardinal"

--T3-L2-Eje5
todosIguales :: Int -> [Int] -> Bool
todosIguales _ [] = False
todosIguales num lista = if (length([y | y <- lista, y == num])) == length(lista) then True else False

--T3-L2-Eje6
mensajeFrase :: String -> String
mensajeFrase texto = "La primera letra de la frase Ejercicios del Tema 3 es " ++ show(head(texto)) ++ " y la ultima letra es " ++ show(last(texto))

--T3-L2-Eje7
rangoNumero :: Int -> String
rangoNumero x =
  let menor = "El número es menor de 10"
      entre = "El número está entre 10 y 20"
      mayor = "El número es mayor de 20"
  in if x < 10 then menor else if x <= 20 then entre else mayor

--T3-L2-Eje17
calificacion :: Double -> String
calificacion x
  | x < 5 = "Suspenso"
  | x < 7 = "Aprobado"
  | x < 9 = "Notable"
  | x < 10 = "Sobresaliente"
  | otherwise = "Matrícula de Honor"

--T3-L2-Eje19
posicionEnLista :: [Int] -> [(Int, Int)]
posicionEnLista xs = zip xs [0..]

--T3-L2-Eje24
partir :: Int -> [Int] -> ([Int],[Int])
partir num lista = (take num lista, drop num lista)

--T3-L2-Eje25
insertar' :: Int -> Int -> [Int] -> [Int]
insertar' n p xs = take p xs ++ [n] ++ drop p xs

--T3-L2-Eje27
listaPotencias :: [Int] -> [Int]
listaPotencias xs = map (\(x, p) -> x^p) (zip (reverse xs) [0..])

--T4-L1-Eje1
cribar :: [Int] -> Int -> [Int]
cribar lista num = [y | y <- lista, not ((y `mod` num) == 0)]

--Recursividad no final
cribar' :: Int -> [Int] -> [Int]
cribar' n xs = cribar'Aux n xs []
    where
        cribar'Aux n [] acc = acc
        cribar'Aux n (x:xs) acc
            | x `mod` n == 0 = cribar'Aux n xs acc
            | otherwise = cribar'Aux n xs (acc ++ [x])

--Recursividad final o de cola
cribar'' :: Int -> [Int] -> [Int]
cribar'' _ [] = []
cribar'' n (x:xs) = if (x `mod` n) == 0 then cribar'' n xs else x : cribar'' n xs

--T4-L1-Eje2
ceros :: [Int] -> Int
ceros [] = 0
ceros (x:xs) = if x == 0 then 1 + ceros xs else ceros xs

--T4-L1-Eje3
repeticiones :: [Int] -> ([Int],[Int])
repeticiones xs = foldr f ([],[]) xs
  where 
    f x (unicos, repetidos)
      | x `elem` unicos = (unicos, x : repetidos)
      | otherwise = (x : unicos, repetidos)

--T4-L1-Eje4
incluye :: [Int] -> [Int] -> Bool
incluye xs ys = isInfixOf xs ys

--T4-L1-Eje5
toLista :: Int -> [Int]
toLista num = map digitToInt (show num)

sumaCifras :: (Int -> [Int]) -> Int -> Int
sumaCifras _ num = foldr (+) 0 (toLista num)

--T4-L1-Eje6
contieneCifra :: (Int -> [Int]) -> Int -> Int -> Bool
contieneCifra _ n cifra = if cifra `elem` (toLista n) then True else False

--T4-L1-Eje7
fromLista :: [Int] -> Int
fromLista = foldl (\acc d -> 10*acc + d) 0

invertir :: (Int -> [Int]) -> ([Int] -> Int) -> Int ->  Int
invertir _ _ n = fromLista (reverse (toLista n))

--T4-L1-Eje8
eliminarUltimos :: Int -> [Int] -> [Int]
eliminarUltimos n xs = take (length xs - n) xs

--Recursividad no final
eliminarUltimos'' :: Int -> [Int] -> [Int]
eliminarUltimos'' n xs = aux xs (length xs - n)
  where 
    aux [] _ = []
    aux (y:ys) k
      | k > 0 = y : aux ys (k-1)
      | otherwise = []

--Recursividad final
eliminarUltimos' :: Int -> [Int] -> [Int]
eliminarUltimos' _ [] = []
eliminarUltimos' n (x:xs) =  if length(eliminarUltimos' n xs) < (length(xs)+1 - n) then x : eliminarUltimos' n xs else []

--T4-L1-Eje9
listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [x] = True
listaOrdenada (x1:x2:xs) = if x1 <= x2 then listaOrdenada (x2:xs) else False

--T4-L2-Eje2
sumaDobles :: [Int] -> Int
sumaDobles [] = 0
sumaDobles (x:xs) = x*2 + sumaDobles xs

sumaDobles' :: [Int] -> Int
sumaDobles' xs = foldr (+) 0 (map (\x -> x*2) xs)

--T4-L2-Eje3
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = foldr (+) 0 (map (\x -> x*x) (filter even xs))

sumaCuadradosPares' :: [Int] -> Int
sumaCuadradosPares' xs = foldr (+) 0 [y*y | y <- xs, even y]

--T4-L2-Eje4
eliminaValor :: Int -> [Int] -> [Int]
eliminaValor n xs = foldr (\x acu -> if x /= n then x : acu else acu) [] xs

--T4-L2-Eje5
eliminaDuplicados :: [Int] -> [Int]
eliminaDuplicados xs = foldr (\x acu -> if x `elem` acu then acu else x : acu) [] xs

eliminaDuplicados' :: [Int] -> [Int]
eliminaDuplicados' xs = foldl (\acu x -> if x `elem` acu then acu else x : acu) [] xs

--T4-L2-Eje6
listaPrimos :: [Int] -> [Int]
listaPrimos [] = []
listaPrimos (x:xs) = if esPrimo x then x : listaPrimos xs else listaPrimos xs

listaPrimos' :: [Int] -> [Int]
listaPrimos' xs = filter esPrimo xs
