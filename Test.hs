import Test.HUnit -- Hunit
import Solucion -- Importamos el archivo general
import CasosDeTest --Importamos los ejemplos

main = runTestTT correrTodo --Correr todos los tests
correrTodo = test [testSuiteEj1, testSuiteEj2, testSuiteEj3, testSuiteEj4, testSuiteEj5, testSuiteEj6, testSuiteEj7] --Test que acumula cada test suite

testSuiteEj1 = test [ --Test suite para el ejercicio1
    "nombresDeUsuarios 1" ~: (nombresDeUsuarios redNormal) ~?= ["Jorge", "Pepe", "Juan"], --Pruebo el caso en el que haya usuarios
    "nombresDeUsuarios 2" ~: (nombresDeUsuarios redVacia) ~?= [], --Pruebo el caso en el que no haya usuarios
    "proyectarNombres 1" ~: (proyectarNombres (usuarios(redConRepetidos))) ~?= ["Jorge", "Pepe"] --Evaluamos el caso con usuarios repetidos.
 ]
testSuiteEj2 = test [
    "amigosDe 1" ~: (amigosDe redUs3DosAmigos usuario3) ~?= [usuario2, usuario4], --Debería devolver us2, us4 porque usuario3 los tiene de amigos.
    "amigosDe 2" ~: (amigosDe redUs3SinAmigos usuario3) ~?= [], --Debería devolver vacío porque usuario3 no se relaciona con nadie.
    "amigosDe 3" ~: (amigosDe redSinAmigos usuario3) ~?= [] --Debería devolver vacío porque la red no tiene relaciones.
 ]
testSuiteEj3 = test [
    "cantidadDeAmigos 1" ~: (cantidadDeAmigos redUs3DosAmigos usuario3) ~?= 2, --Debería devolver 2 porque usuario3 tiene dos amigos.
    "cantidadDeAmigos 2" ~: (cantidadDeAmigos redUs3SinAmigos usuario3) ~?= 0, --Debería devolver 0 porque usuario3 no se relaciona con nadie.
    "cantidadDeAmigos 3" ~: (cantidadDeAmigos redSinAmigos usuario3) ~?= 0 --Debería devolver 0 porque la red no tiene relaciones.  
 ]
testSuiteEj4 = test [
    "usuarioConMasAmigos 1" ~: (usuarioConMasAmigos redUs3DosAmigos) ~?= usuario3, --Debe devolverme usuario3 porque es el que más amigos tiene de la red.
    "usuarioConMasAmigos 2" ~: (usuarioConMasAmigos redConDosMaximos) ~?= usuario1, --Debería devolverme el usuario1 porque es el primero con más amigos.
    "usuarioConMasAmigos 3" ~: (usuarioConMasAmigos redSinAmigos) ~?= usuario1 --Debería devolverme usuario1 porque es el primer usuario con "más" amigos (todos tienen 0 amigos.)
 ]
testSuiteEj5 = test [
    "estaRobertoCarlos 1" ~: (estaRobertoCarlos redConOnceAmigos) ~?= True, --Debe devolver True porque us1 tiene 11 amigos.
    "estaRobertoCarlos 2" ~: (estaRobertoCarlos redConDiezAmigos) ~?= False,--Debe devolver False porque us1 no tiene más de 10 amigos, tiene 10.
    "estaRobertoCarlos 3" ~: (estaRobertoCarlos redSinAmigos) ~?= False --Nadie tiene amigos, entonces nadie tiene más de 10 amigos.
 ]
testSuiteEj6 = test [
    "publicacionesDe 1" ~: (publicacionesDe redUsConDosPubs usuario1) ~?= [publicacion1_1, publicacion1_2], --Devuelve las dos publicaciones de usuario1 en redUsConDosPubs
    "publicacionesDe 2" ~: (publicacionesDe redUsSinPubs usuario1) ~?= [], --El usuario1 no tiene publicaciones en esta red.
    "publicacionesDe 3" ~: (publicacionesDe redSinPubs usuario1) ~?= [] --Ningún usuario de la red tiene pubs.
 ]
testSuiteEj7 = test [
    "publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redUsDiezDosLikes usuario10) ~?= [publicacion7_1, publicacion7_2], --Debe devolverme los dos likes de usuario10.
    "publicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA redUsDiezSinLikes usuario10) ~?= [], --El usuario no le dio like a ninguna publicación.
    "publicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA redSinLikes usuario10) ~?= [] --Nadie le dio like a ninguna publicación.
 ]

testSuiteEj8 = test [
    "lesGustanLasMismasPublicaciones 1" ~: (lesGustanLasMismasPublicaciones )
 ]
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
