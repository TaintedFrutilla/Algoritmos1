--  pinga


-- Ejercicio 1
longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

ultimo :: [t] -> t
ultimo [t] = t
ultimo (x:xs)   | longitud xs == 0 = x
                | otherwise = ultimo xs


-- La funcion principio quiere hacer lo mismo que tail pero lo inverso
principio::[t]->[t]
principio [x] = []
principio (x:xs) = x : principio xs


-- La funcion "reverso" quiere que nos devuelva la lista en el orden inverso
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Ejercicio 2 
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
-- Pues en el vacio no hay ningun elemento
pertenece y (x:xs)  | x == y = True
                    | otherwise = pertenece y xs


todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (y:x:xs) | y /= x = False
                      | otherwise = todosIguales xs

-- Con "todosIguales xs" me estoy quedando con esa cola, y de ahi comenzo a comparar

todosDistintos :: (Eq t) => [t] -> Bool 
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs)   | pertenece x xs = False
                        | otherwise = todosDistintos xs

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

-- Dado un entero y una lista, elimina la primera aparicion del entero en la lista.
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar y (x:xs) | y == x = xs
                | otherwise = x : quitar y xs

-- Dado un entero y una lista, quiero que elimine todas las apariciones de ese entero en la lista.
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos y [] = []
quitarTodos y (x:xs)    | y == x = quitarTodos y xs
                        | otherwise = x : quitarTodos y xs


-- Deja en la lista la unica aparicion de cada elemento, elimina las apariciones restantes
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)
--Voy eliminando a todos los que esten repetidos, agarrando cada elemento, y borrando todas las veces adicionales que aparezca en la lista


-- Dadas dos listas, devuelve verdadero sii ambas listas tienen los mismos elementos, sin tener en cuenta las repeticiones
-- Deberia de tener una funcion quie compare que una sucesion esta contenida en otra

listaContenida :: (Eq t) => [t] -> [t] -> Bool
listaContenida [] ys = True
listaContenida (x:xs) ys = pertenece x ys && listaContenida xs ys

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos (x:xs) ys = listaContenida (x:xs) ys && mismosElementos xs ys 


capicua :: (Eq t) => [t] -> Bool
capicua xs = xs == reverso xs

-- Ejercicio 3 
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

maximo :: [Integer] -> Integer
maximo [a] = a
maximo (y:x:xs) | y >= x = maximo (y:xs)
                | otherwise = maximo (x:xs)

-- Lo que quiero hacer con esta funcion es sumar a todos los elementos de la lista, el numero natural que le inserto
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (n+x):sumarN n xs 

-- Lo que quiero hacer con esta funcion es sumer el primer elemento de la lista al resto de la lista


sumarElPrimero :: [Integer] -> [Integer] 
sumarElPrimero xs = sumarN (head xs) xs

--Lo que quiero hacer con esta funcion es sumar el ultimo elemento de la lista al resto de los elementos de la lista

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs

--pares :: [Integer] -> [Integer]
--multiplosDeN :: Integer -> [Integer] -> [Integer]
--ordenar :: [Integer] -> [Integer]