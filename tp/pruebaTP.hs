module PruebaTP where
import Data.Char

maximo :: Ord a => [a] -> a
maximo [x,y]    | x >= y = x
                | otherwise = y
maximo (x:y:xs) | x <= y = maximo (y:xs)
                | otherwise = maximo (x:xs)


posicion :: (Eq t, Num p) => [t] -> t -> p
posicion (x:xs) a | a == x = 0
                  | otherwise = 1 + posicion xs a
                  
valorDeLaPosicion :: (Eq t, Num t) => [p] -> t -> p
valorDeLaPosicion (x:xs) n | n == 1 = x
                           | otherwise = valorDeLaPosicion xs (n-1)


listaConMinusculas :: String -> Bool
listaConMinusculas [] = False
listaConMinusculas (x:xs) | esMinuscula x = True
                          | otherwise = listaConMinusculas xs
                          
-- Ejercicio 1
esMinuscula :: Char -> Bool
esMinuscula a = ord a >= 97 && ord a <= 122


--Los casos base que considere lo hice segun los asegura de las especificaciones
{- Casos de test (Lu)
1. "a"
2. "z"
3. "minuscula entremedia"
4. "ñ"
5. "letra con tilde"
-}


-- Ejercicio 2
letraANatural :: Char -> Int
letraANatural a = ord a - 97

{-Casos de test (Lu)
1. "a = 0"
2. "z = 25"
3. "letra intermedia"
-}

-- Ejercicio 3
desplazar :: Char -> Int -> Char
desplazar a n | not (esMinuscula a) = a
              | letraANatural a + n <= 25 && letraANatural a + n >= 0 = chr (97 + letraANatural a + n)
              | letraANatural a + n < 0 = chr (123 + letraANatural a + n)
              | otherwise = desplazar a (n-26)

{-
Casos de test:
"letra para adelante"
"0 misma letra"
"z y 1 te devuelve a"
"a y -1 te devuelve z"
-}

{-
Casos de test (Lu)
1. "desplazamiento no minuscuala"= ninguno
2. "0 misma letra"
3. "z y 1 te devuelve a"
4. "letra entre 0 y 25" (discutible)
5. "alguna vuelta positiva"
6. "a y -1 te devuelve z"
7. "letra entre -1 y -25" (discutible)
8." alguna vuelta negativa"
-}
              
              
-- Ejercicio 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n = desplazar x n : cifrar xs n


{- Casos de test (Lu)
1. "probar el vacio"
2. "numero cero"
3. "palabra minuscula"
4. "palabra con alguna no minuscula"
5. "mas de una palabra para ver que ondis los espacios"
-}


{-Casos de test
cifrar "" 1 = ""
cifrar "
-}

-- Ejercicio 5
--requiere n>=0
descifrar :: String -> Int -> String
descifrar "" _ = ""
descifrar (x:xs) n = desplazar x (-n) : descifrar xs n

{- Casos de test (Lu)
Los mismos que en el ejercicio anterior
-}

-- Ejercicio 6
cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (x:xs) n = cifrar x n : cifrarListaAux xs (n+1)


cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista (x:xs) = cifrarListaAux (x:xs) 0

{- Casos de test
1. Caso base
2. Lista con una palabra
3. Lista con mas palabras con alguna palabra que contenga no minusculas -}
-- No se me ocurrio mucho mas, si alguno tiene una sugerencia bienvenida es.

-- Ejercicio 7 (Segun los casos de prueba, anda todillo)
cantidadDeUnaLetra :: String -> Char -> Int
cantidadDeUnaLetra [] _ = 0
cantidadDeUnaLetra (x:xs) a | a == x && esMinuscula a = 1 + cantidadDeUnaLetra xs a
                            | otherwise = cantidadDeUnaLetra xs a


listaSoloMinusculas :: String -> String
listaSoloMinusculas [] = []
listaSoloMinusculas (x:xs) | esMinuscula x = x:listaSoloMinusculas xs
                           | otherwise = listaSoloMinusculas xs


porcentajeDeUnaLetra :: String -> Char -> Float
porcentajeDeUnaLetra palabra a  | not (listaConMinusculas palabra) = 0.0
                                | otherwise = (fromIntegral (cantidadDeUnaLetra palabra a) / fromIntegral (length (listaSoloMinusculas 
                                palabra))) * 100


frecuenciaAux :: String -> Int -> [Float]
frecuenciaAux _ 123 = []
frecuenciaAux palabra n = [porcentajeDeUnaLetra palabra (chr n)] ++ frecuenciaAux palabra (n+1)


frecuencia :: String -> [Float]
frecuencia palabra = frecuenciaAux palabra 97


{--}
{-Casos de test
1. "vacio"
2. "repeticion de una letra"
3. "palabra invalida"
4. "palabra con minusculas y caracteres invalidos"
5. "una palabra"
6. "tres palabras"
-}
   
-- Ejercicio 8            
maximoDeLaFrecuencia :: String -> Int -> Float
maximoDeLaFrecuencia palabra n = maximo (frecuencia (cifrar palabra n))


cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente palabra n   | listaConMinusculas palabra = (chr (posicion (frecuencia (cifrar palabra n)) (maxi) + 97), maxi)
                                    where maxi = maximoDeLaFrecuencia palabra n
               
{- Casos de test (Lu)
1. Una sola letra c/ n. positivo.
2. Palabra c/ n. positivo.
3. Palabra c/ n. negativo.
4. Palabra con letras no minusculas y minusculas.
5. palabra con mas de una frecuencia igual.
-}
               
-- Ejercicio 9
esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux [] b _ = False
esDescifradoAux a b n | cifrar a n == b = True
                      | n <= 25 = esDescifradoAux a b (n+1)
                      | otherwise = False
                      

esDescifrado :: String -> String -> Bool
esDescifrado a b = esDescifradoAux a b 0

{- Casos de test (Lu)
1. Vacio con el vacio deberia de dar false?
2. Vacio con otra lista = False
3. Lista con Vacio = False
4. Listas con distintas longitudes = False
5. Cifrado = True
-}

-- Ejercicio 10
descifradoCabeza :: [String] -> [(String, String)]
descifradoCabeza [x] = []
descifradoCabeza (x:y:xs) | esDescifrado x y = (x,y) : descifradoCabeza (x:xs)
                          | otherwise = descifradoCabeza (x:xs)

eliminarElementoPosicion [] _ = []
eliminarElementoPosicion (x:xs) n | n==1 = xs
                                  | otherwise = x: eliminarElementoPosicion xs (n-1)


moverPosicionACabeza :: [String] -> Int -> [String]
moverPosicionACabeza [] _ = []
moverPosicionACabeza (x:xs) n = valorDeLaPosicion (x:xs) n : eliminarElementoPosicion (x:xs) n


descifradoDeLaPosicion :: [String] -> Int -> [(String, String)]
descifradoDeLaPosicion l n = descifradoCabeza (moverPosicionACabeza l n)


todosLosDescifradosAux :: [String] -> Int -> [(String, String)]
todosLosDescifradosAux [] _ = []
todosLosDescifradosAux l n | n == length l = descifradoDeLaPosicion l n
                           | otherwise = descifradoDeLaPosicion l n ++ todosLosDescifradosAux l (n+1)


todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados l = todosLosDescifradosAux l 1


{- Casos de test
1. Vacio = Vacio
2. palabra solitario = vacio
3. 2 palabras iguales = lista de 2 tuplas
4. 2 palabras iguales y una extra = lo mismo que el anterior
5. dos pares de palabras iguales = lista de 4 tuplas, dos y dos :)
6. 5 palabras, 3 distintas, 2 iguales
-}

--Ejercicio 11

expandirClave :: String -> Int -> String
expandirClave clave n = tomarN n (expandirClaveAux clave n)

{--toma una clave y un numero n y devuelve una nueva cadena con la clave repetida--}

expandirClaveAux :: String -> Int -> String
expandirClaveAux [] _ = []
expandirClaveAux clave n = clave ++ expandirClaveAux clave (n-length clave)

{-expande la clave concatenandola consigo misma hasta que su longitud sea al menos n-}

tomarN :: Int -> [a] -> [a]
tomarN 0 _ = []
tomarN _ [] = []
tomarN n (x:xs) = x : tomarN (n-1) xs
{--toma los primeros n elementos de una lista. Esto me permite asegurar que la longitud final sea n--}

{- Casos de test
1. n=1 y |clave|=1 , devuelve la letra de la clave
2. n=4 y clave = 1 , repeticion de la clave 4 veces
3. n=1 y clave = 5 , devuelve la primera letra de la clave
4. n= 7 y clave =4 , se repoite la clave
-}

--Ejercicio 12

desplazar02 :: Char -> Int -> Char
desplazar02 c n | 'a' <= c && c <= 'z' = chr (((ord c - ord 'a' + n) `mod` 26) + ord 'a')
                | otherwise = c  -- Para caracteres que no son letras

--cifrar02 :: String -> String -> String
--cifrar02 _ [] = []
--cifrar02 (x:xs) (k:ks) = desplazar x (ord k - ord 'a') : cifrar xs ks

--cifrarVigenere :: String -> String -> String
--cifrarVigenere s clave = cifrar s (expandirClave clave (length s))

{- Casos de test
1. palabra y clave con "a" = misma palabra 
2. palabra y clave con "b" = desplazar palabra 1
3.

-}


--Ejercicio 13

{-desplazarinverso :: Char -> Int -> Char
desplazarInverso c n 
                     | 'a' <= c && c <= 'z' = chr (((ord c - ord 'a' - n) `mod` 26) + ord 'a')
                     | 'A' <= c && c <= 'Z' = chr (((ord c - ord 'A' - n) `mod` 26) + ord 'A')
                     | otherwise = c  -- Para caracteres que no son letras

descifar02:: String -> String -> String
descifrar02 [] _ = []
descifrar02 _ [] = []
descifrar02 (x:xs) (k:ks) = desplazarInverso x (ord k - ord 'a') : descifrar02 xs ks

descifrarVigenere :: String -> String -> String
descifrarVigenere s clave = descifrar02 s (expandirClave clave (length s))

--Ejercicio 14

distancia :: String -> String -> Int
distancia [] [] = 0
distancia (x:xs) (y:ys) = abs (letraANatural x - letraANatural y) + distancia xs ys

peorCifrado :: String -> [String] -> String
peorCifrado s (c:cs) = buscarClave s cs c (distancia s (cifrarVigenere s c))

buscarClave :: String -> [String] -> String -> Int -> String
buscarClave _ [] mejorClave _ = mejorClave
buscarClave s (clave : claves) mejorClave mejorDist
                             | distActual < mejorDist = buscarClave s claves clave  distActual
                             |otherwise = buscarClave s claves mejorClave mejorDist

                        where distActual = distancia s (cifrarVigenere s clave)

--Ejercicio 15 

--combinacionesVigenere :: [String] -> [String] -> String -> [(string, string)]
--combinacionesVIgenere msjs claves cifrado = [(m,k)] | n >= msjs , k <= claves, cifrarVigenere n k == cifrado)

-}