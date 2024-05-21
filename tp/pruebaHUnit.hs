import Test.HUnit

--La funcion que queremos probar
valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x
                | otherwise = -x


--Las pruebas unitarias
testSuiteValorAbs = test[
    " casoPositivo" ~: (valorAbsoluto 1) ~?= 1 ,
    " casoNegativo" ~: (valorAbsoluto (-5)) ~?= 5,
    " casoCero" ~: (valorAbsoluto 0) ~?= 0]


--Corre todas las pruebas
run = runTestTT testSuiteValorAbs