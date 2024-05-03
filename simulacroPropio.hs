-- Ejercicio 1

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs)  | y == x = True
                    | otherwise = pertenece y xs 

mismaPersona :: (String, String) -> Bool
mismaPersona (a,b) = a ==b

invertir :: (String, String) -> (String, String)
invertir (a,b) = (b,a)

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs)    | mismaPersona x = False
                            | pertenece (invertir x) xs = False
                            | otherwise = relacionesValidas xs

-- Ejercicio 2

--personas :: [(String, String)] -> [String]
--personas [] =


lista :: (String,String) -> [String]
lista (a,b)=[a,b]

grandelista :: [(String,String)] -> [String]
grandelista [] = []
grandelista (x:xs) = (lista x) ++ grandelista xs



