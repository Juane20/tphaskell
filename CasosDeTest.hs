module CasosDeTest where
--Ejemplos generales
usuario1 = (1, "Jorge")
usuario2 = (2, "Pepe")
usuario3 = (3, "Juan")
usuario4 = (4, "Francisco")
usuario5 = (5, "Natalia")
usuario6 = (6, "Joaquin")
usuario7 = (7, "Jorge")
usuario8 = (8, "Messi")
usuario9 = (9, "Martín")
usuario10 = (10, "Elias")
usuario11 = (11, "Angel")
usuario12 = (12, "Kevin")

relacion1_2 = (usuario1, usuario2) --(us1, us2) == (us2, us1) entonces no armamos las antisimétricas.
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario1, usuario4)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)
relacion2_3 = (usuario2, usuario3)
relacion2_4 = (usuario2, usuario4)
relacion2_5 = (usuario2, usuario5)
relacion2_6 = (usuario2, usuario6)
relacion2_7 = (usuario2, usuario7)
relacion3_4 = (usuario3, usuario4)
relacion3_5 = (usuario3, usuario5)
relacion3_6 = (usuario3, usuario6)
relacion4_5 = (usuario4, usuario5)
relacion4_6 = (usuario4, usuario6)
relacion5_6 = (usuario5, usuario6)
relacion7_9 = (usuario7, usuario9)
relacion7_3 = (usuario7, usuario3)
relacion7_10 = (usuario7, usuario10)
relacion7_11 = (usuario7, usuario11)
relacion8_10 = (usuario8, usuario10)
relacion9_10 = (usuario9, usuario10)
relacion6_7 = (usuario6, usuario7)
relacion6_8 = (usuario6, usuario8)
relacion7_8 = (usuario7, usuario8)
relacion8_9 = (usuario8, usuario9)
relacion9_6 = (usuario9, usuario6)

publicacion1_1 = (usuario1, "Me llamo Jorge.", [usuario2]) --En este ejercicio, no se usan las publicaciones, ponemos ejemplos genéricos.
publicacion1_2 = (usuario1, "Me llamo Jorge. Y este es mi segundo ejemplo", [usuario2]) --En este ejercicio, no se usan las publicaciones, ponemos ejemplos genéricos.
publicacion2_1 = (usuario2, "Soy Pepe y esta es mi primera publicación", [usuario1])
publicacion2_2 = (usuario2, "Soy Pepe y esta es mi segunda publicación", [usuario1])
publicacion2_3 = (usuario2, "Mi tercera publicación", [usuario2, usuario4])
publicacion4_1 = (usuario4, "Soy Francisco! :)", [usuario5, usuario6])
publicacion4_2 = (usuario4, "Hola gente!", [usuario6, usuario5])
publicacion4_3 = (usuario4, "Soy Francisco v2! :)", [usuario6])
publicacion4_4 = (usuario4, "Hola gente v2!", [usuario5])
publicacion5_1 = (usuario5, "Soy Pepe y esta es mi segunda publicación", [usuario1,usuario12])
publicacion7_1 = (usuario7, "Soy Messi", [usuario9, usuario10])
publicacion7_2 = (usuario7, "Aguante Newells", [usuario10, usuario11])
publicacion7_3 = (usuario7, "Sigo siendo Messi", [usuario9])
publicacion7_4 = (usuario7, "Aguante Central", [usuario11])
publicacion9_1 = (usuario1, "Quiero tener un seguidor fiel!", [usuario2, usuario3])
publicacion9_2 = (usuario1, "Gracias usuario3", [usuario3])
publicacion9_3 = (usuario1, "No tengo un seguidor fiel :(", [usuario3])
publicacion9_4 = (usuario1, "Qué malos que son", [usuario2])
publicacion10_1 = (usuario9, "¿Me relaciono con el usuario 6?", [usuario8])
publicacionUnoSinLikes = (usuario8, "Quiero 1 like", [])
publicacionDosSinLikes = (usuario9, "Nadie me da like.", [])


--Ejercicio 1
--nombresDeUsuarios
usuarios1_1 = [usuario1, usuario2, usuario3]
relaciones1_1 = [relacion1_2, relacion2_3] --En este ejercicio, no se usan las relaciones, ponemos ejemplos genéricos.
publicaciones1_1 = [publicacion1_1]
redNormal = (usuarios1_1, relaciones1_1, publicaciones1_1) --Una red con usuarios.
redVacia = ([], [], []) --Red vacía, sin usuarios (por especificación, es válida.)
--proyectarNombres
usuarios1_2 = [usuario1, usuario2, usuario7]
relaciones1_2 = [relacion1_2, relacion2_7] --En este ejercicio, no se usan las relaciones, ponemos ejemplos genéricos.
publicaciones1_2 = [publicacion1_2]
redConRepetidos = (usuarios1_2, relaciones1_2, publicaciones1_2)

--Ejercicio 2,3,4
--amigosDe
usuarios2_1 = [usuario1, usuario2, usuario3, usuario4]
relaciones2_1 = [relacion2_3, relacion3_4] --Us3 tiene 2 amigos.
publicaciones2_1 = [publicacion2_1] --Publicación de relleno.
redUs3DosAmigos = (usuarios2_1, relaciones2_1, publicaciones2_1) --Red con 3 usuarios, donde un usuario tiene 2 amigos (usuario3 tiene de amigos a usuario2 y usuario4)

relaciones2_2 = [relacion1_2, relacion2_4]
publicaciones2_2 = [publicacion2_2] --Publicación de relleno.
redUs3SinAmigos = (usuarios2_1, relaciones2_2, publicaciones2_2)

redSinAmigos = (usuarios2_1, [], publicaciones2_2) -- Red sin relaciones

relaciones2_3 = [relacion1_2, relacion2_4, relacion1_3]
publicaciones2_3 = [publicacion2_3] --Publicación de relleno.
redConDosMaximos = (usuarios2_1, relaciones2_3, publicaciones2_3)
--Ejercicio 5
usuarios5_1 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relaciones5_1 = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11, relacion1_12] --Usuario 1 tiene 11 amigos
relaciones5_2 = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11] --Usuario 1 tiene 11 amigos

publicaciones5_1 = [publicacion5_1] --Publicación de relleno.

redConOnceAmigos = (usuarios5_1, relaciones5_1, publicaciones5_1) --us1 tiene 11 amigos
redConDiezAmigos = (usuarios5_1, relaciones5_2, publicaciones5_1) --us1 tiene 10 amigos

--Ejercicio 6
usuarios6_1 = [usuario1, usuario2]
relaciones6_1 = [relacion1_2]
publicaciones6_1 = [publicacion1_1, publicacion1_2]
redUsConDosPubs = (usuarios6_1, relaciones6_1, publicaciones6_1) --Red donde us1 tiene las publ 1_1 y 1_2

publicaciones6_2 = [publicacion2_1]
redUsSinPubs = (usuarios6_1, relaciones6_1, publicaciones6_2) --Red donde us1 no tiene pubs.

redSinPubs = (usuarios6_1, relaciones6_1, []) --Red sin publicaciones.

--Ejercicio 7
usuarios7_1 = [usuario7, usuario9, usuario10, usuario11]
relaciones7_1 = [relacion7_9, relacion7_10, relacion7_11, relacion8_10, relacion9_10]
publicaciones7_1 = [publicacion7_1, publicacion7_2]
publicaciones7_2 = [publicacion7_3, publicacion7_4]
redUsDiezDosLikes = (usuarios7_1, relaciones7_1, publicaciones7_1) --El usuario 10 le dio like a las dos publicaciones de usuario7 (publicacion7_1, publicacion7_2)

redUsDiezSinLikes = (usuarios7_1, relaciones7_1, publicaciones7_2) --El usuario 10 no le dio like a ninguna publicación.

publicacionesSinLikes = [publicacionUnoSinLikes, publicacionDosSinLikes] --Publicaciones sin likes
redSinLikes = (usuarios7_1, relaciones7_1, publicacionesSinLikes) --Red sin likes

--Ejercicio 8
usuarios8_1 = [usuario4, usuario5, usuario6]
relaciones8_1 = [relacion4_5, relacion4_6]
publicaciones8_1 = [publicacion4_1, publicacion4_2] --Dos publicaciones donde, tanto usuario5 como usuario6, le dieron like a ambas.
redConDosUsMismosLikes = (usuarios8_1, relaciones8_1, publicaciones8_1) --Red donde los dos usuarios tienen los mismos likes.

publicaciones8_2 = [publicacion4_3, publicacion4_4] --Dos publicaciones con likes totalmente diferentes.
redSinDosUsMismosLikes = (usuarios8_1, relaciones8_1, publicaciones8_2) --Red donde las dos publicaciones tienen likes diferentes, por tanto, no hay dos usuarios que le hayan dado like a las mismas publicaciones.

--Ejercicio 9
usuarios9_1 = [usuario1, usuario2, usuario3]
relaciones9_1 = [relacion1_2, relacion1_3]
publicaciones9_1 = [publicacion9_1, publicacion9_2]
redConSeguidorFiel = (usuarios9_1, relaciones9_1, publicaciones9_1) --Red donde el usuario 3 le dio like a las dos publicaciones de usuario1

publicaciones9_2 = [publicacion9_3, publicacion9_4]
redSinSeguidorFiel = (usuarios9_1, relaciones9_1, publicaciones9_2) --Red donde ningún usuario le dio like a todas las publicaciones del usuario1.

--Ejercicio 10
usuarios10_1 = [usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9]
relaciones10_1 = [relacion6_7, relacion7_8, relacion8_9] --usuario 6 se relaciona con 7, 7 con 8, 8 con 9, entonces, existe una secuencia de amigos entre 6 y 9
publicaciones10_1 = [publicacion10_1] --Publicación de relleno
redConSecuencia = (usuarios10_1, relaciones10_1, publicaciones10_1) --Red donde existe una secuencia de amigos entre usuario6 y usuario9

relaciones10_2 = [relacion6_7, relacion7_8, relacion4_5] -- El usuario 9 no tiene amigos entonces, dará false. Además, el 6 no se relaciona con el 5 porque no existe ninguna secuencia de amigos que los relacione.
redSinSecuencia = (usuarios10_1, relaciones10_2, publicaciones10_1)
