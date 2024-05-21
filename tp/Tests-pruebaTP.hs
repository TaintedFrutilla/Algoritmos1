import Test.HUnit
import PruebaTP
import Data.List

run = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave
--    "cifrarVigenere" ~: testsEjcifrarVigenere,
--    "descifrarVigenere" ~: testsEjdescifrarVigenere,
--    "peorCifrado" ~: testsEjpeorCifrado,
--    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


-- Ejercicio 1
testsEjesMinuscula = test [
    "minuscula1"~:(esMinuscula 'a')~?= True,
    "minuscula2"~:(esMinuscula 'z')~?= True,
    "minuscula3"~:(esMinuscula 'm')~?= True,
    "enie"~:(esMinuscula 'ñ')~?= False,
    "letraConTilde"~:(esMinuscula 'ó')~?= False
    ]

-- Ejercicio 2
testsEjletraANatural = test [
    "numeroCero"~:(letraANatural 'a')~?= 0,
    "numeroVeinticinco"~:(letraANatural 'z')~?=25,
    "numeroIntermedio"~:(letraANatural 'l')~?=11
    ]

-- Ejercicio 3
testsEjdesplazar = test [
    "noDesplazanoMinuscula"~:(desplazar 'ñ' 3)~?= 'ñ',
    "noDesplazaMinuscula"~:(desplazar 'b' 0)~?= 'b',
    "primerDesplazamiento"~:(desplazar 'z' 1)~?= 'a',
    "desplazaPositivo"~:(desplazar 'c' 3)~?= 'f',
    "desplazaConVueltaPositiva"~:(desplazar 'e' 33)~?= 'l',
    "ultimoDesplazamiento"~:(desplazar 'a' (-1))~?= 'z',
    "desplazaNegativo"~:(desplazar 'n' (-7))~?= 'g',
    "desplazaConVueltaNegativa"~: (desplazar 'a' (-1))~?= 'z'
    ]

-- Ejercicio 4
testsEjcifrar = test [
    "casoBase"~:(cifrar "" 3)~?= "",
    "noSeDesplaza"~:(cifrar "algoritmo" 0)~?= "algoritmo",
    "todoMinuscula"~:(cifrar "supernova" 3)~?= "vxshuqryd",
    "algunaNoMinuscula"~:(cifrar "ñandú" 3)~?= "ñdqgú",
    "masDeUnaPalabra"~:(cifrar "trabajo practico" 5)~?= "ywfgfot uwfhynht"
    ]

-- Ejercicio 5
testsEjdescifrar = test [
    "casoBase"~:(descifrar "" 5)~?= "",
    "noDescifra"~:(descifrar "palabra" 0)~?= "palabra",
    "todoMinuscula"~:(descifrar "vxshuqryd" 3)~?= "supernova",
    "algunaNoMinuscula"~:(descifrar "ñdqgú" 3)~?= "ñandú",
    "masDeUnaPalabra"~:(descifrar "ywfgfot uwfhynht" 5)~?= "trabajo practico"
    ]


-- Ejercicio 6
testsEjcifrarLista = test [
    "listaVacia"~:(cifrarLista [])~?= [],
    "unElemento"~:(cifrarLista ["perro"])~?= ["perro"],
    "variosElementos"~:(cifrarLista ["letra", "boca", "ñandú"])~?= ["letra", "cpdb", "ñcpfú"]
    ]

-- Ejercicio 7
testsEjfrecuencia = test [
    "palabraVacia"~:(frecuencia "")~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], 
    "letraRepetida"~:(frecuencia "aaaa")~?=[100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "palabraInvalida"~:(frecuencia "áéóñíú")~?=[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    -- Me da que falla, que macana (revisar)
    -- Depende de como lo ponga, de momento no me anda la neurona para ver que pasa.
    "palabraCombinada"~: expectlistProximity (frecuencia "papá") [33.333336,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,66.666667,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "palabrita"~: expectlistProximity (frecuencia "bombonera") [11.111111,22.222222,0.0,0.0,11.111111,0.0,0.0,0.0,0.0,0.0,0.0,0.0,11.111111,11.111111,22.222222,0.0,0.0,11.111111,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
   
    --"probando" ~: expectlistProximity (frecuencia "") [0,0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    
    "palabritas"~: expectlistProximity (frecuencia "mi peor enemigo") [0.0,0.0,0.0,0.0,23.076923,0.0,7.692307,0.0,15.384615,0.0,0.0,0.0,15.384615,7.692307,15.384615,7.692307,0.0,7.692307,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    -- Tiene tolerancia con 16.66666 pero no con 16.6666 (reveer)
    --expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

-- Ejercicio 8
-- Dice que n deberia ser entero, pendiente los casos negativos
testsEjcifradoMasFrecuente = test [
    "letraSola"~:(cifradoMasFrecuente "p" 3)~?= ('s',100.0),
    "palabraNumeroPosito"~: expectAnyTuplaAprox (cifradoMasFrecuente "menta" 5) [('r',20.0), ('j',20.0), ('s',20.0), ('y',20.0), ('f',20.0)],
    "palabraNumeroNegativo"~: expectAnyTuplaAprox (cifradoMasFrecuente "yoga" (-2)) [('w',25.0),('m',25.0),('e',25.0),('y',25.0)],
    "palabramixta"~: expectAnyTuplaAprox (cifradoMasFrecuente "muñecas" 20) [('g',16.666666),('o',16.666666),('y',16.666666),('w',16.666666),('u',16.666666),('m',16.666666)],
    "frecuenciasIguales" ~: expectAnyTuplaAprox(cifradoMasFrecuente "bizcochitos" 3) [('l',18.181818),('f',18.181818),('r',18.181818)]
    ]  

-- Ejericicio 9
testsEjesDescifrado = test [
    "vacioConVacio"~:(esDescifrado "" "")~?= (False),
    "vacioConPalabra"~:(esDescifrado "" "necrolimbo")~?= (False),
    "palabraConVacio"~:(esDescifrado "necrolimbo" "")~?= (False),
    "distintasLongitudes"~:(esDescifrado "gideon" "ranni")~?= (False),
    "palabraCifrada"~:(esDescifrado "neuron" "arheba")~?= (True)
    ]

--Ejercicio 10
-- expectPermutation 
testsEjtodosLosDescifrados = test [
    "vacio"~:(todosLosDescifrados [])~?= ([]),
    "palabra solita"~:(todosLosDescifrados ["jungla"])~?=([]),
    "dosPalabrasIgualesSolas"~:True ~=? esPermutacion [("etgujbt","perfume"),("perfume","etgujbt")] (todosLosDescifrados ["perfume","etgujbt"]),
    "dosPalabrasIgualesConExtras"~:True ~=? esPermutacion [("etgujbt","perfume"),("perfume","etgujbt")] (todosLosDescifrados ["tivwsre","perfume","hsqmrks","etgujbt"]),
    --palabras: persona, perfume (+15)(x2), domingo
    "dosParesPalabrasIguales"~:True ~=? esPermutacion (todosLosDescifrados ["qyrhsw","cpknnq","tbukvz","jwruux"]) [("qyrhsw","tbukvz"),("cpknnq","jwruux"),("tbukvz","qyrhsw"),("jwruux","cpknnq")],
    -- mundos (+4 y +7) y anillo (+2 y +9)
    "dosPalabrasIguales"~:True ~=? esPermutacion (todosLosDescifrados ["ttcdqi","qyrhsw","kqclil","tbukvz","lwzuqz"]) [("tbukvz","qyrhsw"),("qyrhsw","tbukvz")]
    -- mundos (+4 y +7), lluvia, ciudad, dormir (+8)
    ]

-- Ejercicio 11
testsEjexpandirClave = test [
    "caspBase"~:(expandirClave "t" 1)~?=("t"),
    "claveUnitariaRepetida"~:(expandirClave "r" 5)~?=("rrrrr"),
    "primeraLetraClave"~:(expandirClave "boca" 1)~?=("b"),
    "repeticionClave"~:(expandirClave "pato" 7)~?=("patopat")
    ]

-- Ejercicio 12
{-testsEjcifrarVigenere = test  [
    "loMismo"~:(cifrarVigenere "histeria" "a")~?=("histeria"),
    "unDesplazamiento"~:(cifrarVigenere "histeria" "b")~?=(cifrar "histeria" 1),
    "muchosDesplazamientos"~:(cifrarVigenere "histeria" "muse")~?=("tckxqlae")
]

-- Ejercicio 13
--testsEjdescifrarVigenere = test [
    "loMismo"~:(descifrarVigenere "ijtufsjb" "a")~?=("ijtufsjb"),
    "unDesplazamiento"~:(descifrarVigenere "ijtufsjb" "b")~?=(descifrar "histeria" 1),
    "muchosDesplazamientos"~:(descifrarVigenere "tckxqlae" "muse")~?=("histeria")
]

-- Ejercicio 14
--testsEjpeorCifrado = test []

-- Ejercicio 15
--testsEjcombinacionesVigenere = test []






-- Funciones utiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)