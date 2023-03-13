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



