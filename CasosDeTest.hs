module CasosDeTest where
--Ejercicio 1 (proyectarNombres y nombresDeUsuarios)
usuario1Ej1 = (1, "Natasha")
usuario2Ej1 = (2, "Elias")
usuario3Ej1 = (3, "Bere")

relacion1_2Ej1 = (usuario1Ej1, usuario2Ej1)
relacion2_3Ej1 = (usuario2Ej1, usuario3Ej1)

publicacion1Ej1 = (usuario1Ej1, "Soy Natasha y me gusta el pole.", [usuario1Ej1])

usuarios1Ej1 = [usuario1Ej1, usuario2Ej1, usuario3Ej1]
publicacionesEj1 = [publicacion1Ej1]
relacionesEj1 = [relacion1_2Ej1, relacion2_3Ej1]


redConUsuariosEj1 = (usuarios1Ej1, relacionesEj1, publicacionesEj1)
redVaciaEj1 = ([], [], [])