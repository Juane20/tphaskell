import Test.HUnit
import Solucion
import CasosDeTest
main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redConUsuariosEj1) ~?= ["Natasha", "Elias", "Bere"], --Pruebo el caso en el que haya usuarios
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redVaciaEj1) ~?= [] --Pruebo el caso en el que no haya usuarios
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
