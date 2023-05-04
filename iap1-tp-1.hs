-- Completar con los datos del grupo
--
-- Nombre de Grupo: KSEA_team
-- Integrante 1: Kevin Ezequiel La Rocca, kevinlarocca2000@gmail.com, 874/23
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)



type Usuario = (Integer, String) -- (id, nombre) Ej (1, "Kevin")     [1,2] [2,1]
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan Ej ((6, "Kevin"), (2, "Angel"), ((2, "Angel"), (6, "Kevin")) Esto solo contiene UN ELEMENTO
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
-- ((1, "Kevin"), "Casa", [(2, "Angel"), (3, "Sofia")])
type RedSocial = ([Usuario], [Relacion], [Publicacion])
-- ([(1, "Kevin"), (2, "Angel"), (3, "Sofia")], [])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Predicados/Funciones auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool -- Pertenece el elemento de tipo t a la lista de elementos de tipo t
pertenece _ [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs

empiezaCon :: (Eq t) => t -> [t] -> Bool -- Comprueba si el elemento de tipo t es el primer elemento de la lista de tipo t.
empiezaCon _ [] = False
empiezaCon x (y:ys) = x == y

ultimoElemento :: (Eq t) => [t] -> t -- Dada una lista, devuelve el último elemento.
ultimoElemento [] = undefined
ultimoElemento [x] = x
ultimoElemento (x:xs) = ultimoElemento xs

terminaCon :: (Eq t) => t -> [t] -> Bool -- Comprueba si el elemento de tipo t es el último elemento de la lista de tipo t.
terminaCon _ [] = False
terminaCon x (y:ys) = ultimoElemento(y:ys) == x

sinRepetidos :: (Eq t) => [t] -> Bool -- Comprueba que una lista no tenga repetidos
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs = False
                    | otherwise = sinRepetidos xs

quitar :: (Eq t) => t -> [t] -> [t] -- Devuelve la lista quitando una vez el elemento.
quitar _ [] = undefined
quitar x (y:ys)
    | x /= y = y:quitar x ys
    | otherwise = ys

mismosElementos :: (Eq t) => [t] -> [t] -> Bool -- Comprueba que dos listas tengan los mismos elementos (sin repetidos)
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ = False amigosDe x y
    | if pertenece y
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) = mismosElementos xs (quitar x (y:ys))
                              | otherwise = False
                              
usuarioValido :: Usuario -> Bool --(1, "Kevin") -- Comprueba que un usuario sea válido.
usuarioValido a | idDeUsuario a > 0 && nombreDeUsuario a /= "" = True
                | otherwise = False

concatenarIdDeUsuarios :: [Usuario] -> [Integer] -- Concateno en una lista los ID de los usuarios.
concatenarIdDeUsuarios [] = []
concatenarIdDeUsuarios (x:xs) = (idDeUsuario(x):concatenarIdDeUsuarios(xs))

noHayIdsRepetidos :: [Usuario] -> Bool -- Comprueba que, dada una lista de usuarios, no haya id's repetidos.
noHayIdsRepetidos [] = True
noHayIdsRepetidos x = sinRepetidos(concatenarIdDeUsuarios x)

usuariosValidos :: [Usuario] -> Bool --[(1, "Kevin"), (2, "Angel")etc...] -- Comprueba que, dada una lista de usuarios,todos los usuarios sean válidos.
usuariosValidos [] = False
usuariosValidos [x] = usuarioValido x
usuariosValidos (x:xs) | usuarioValido x && noHayIdsRepetidos (x:xs) = usuariosValidos xs
                       | otherwise = False

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool--(a,b) a = 0 = (1, "Kevin"), b = 1 = (2, "Angel")
usuariosDeRelacionValidos [] [] = True
usuariosDeRelacionValidos _  [] = True 
usuariosDeRelacionValidos [] _  = False
usuariosDeRelacionValidos (x:xs) (y:ys) = (pertenece (fst y) (x:xs) && pertenece (snd y) (x:xs) && fst y /= snd y)
    && usuariosDeRelacionValidos (x:xs) ys
{-Ejemplos: [(1, "Kevin"), (2, "Angel"), (3, "Sofia"), (4, "Elias")] ----> Lista de tipo Usuario Tiene 4 elementos
            [((1,"Kevin"), (2,"Angel")), ((3, "Sofia"), (4, "Elias")), ((1, "Pepito"), (2, "Gomez"))] ---> Lista de tipo Relacion. Tiene 2 elementos-}

relacionesAsimetricas :: [Relacion] -> Bool 
relacionesAsimetricas [] = True
relacionesAsimetricas (x:xs) | pertenece (snd x, fst x) (x:xs) = False
                             | otherwise = relacionesAsimetricas xs

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas [x] = True
noHayRelacionesRepetidas (x:y:xs) | (idDeUsuario(fst x) /= idDeUsuario(fst y)) || (idDeUsuario(snd x) /= idDeUsuario(snd y)) = noHayRelacionesRepetidas (x:xs)
                                  | otherwise = False

relacionadoDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadoDirecto _ _ (_, _, []) = False 
relacionadoDirecto a b (cs, d:ds, fs) | pertenece (a,b) (d:ds) || pertenece (b,a) (d:ds) = True
                                     | otherwise = relacionadoDirecto a b (cs, ds, fs)
-- Ejercicios 

proyectarNombres :: [Usuario] -> [String] -- Dada una lista de usuarios, devuelvo los nombres.
proyectarNombres [] = []
proyectarNombres (x:xs) = (nombreDeUsuario x:proyectarNombres xs)

nombresDeUsuarios :: RedSocial -> [String] -- Dada una red social, devuelvo los nombres de los usuarios.
nombresDeUsuarios x = proyectarNombres(usuarios(x))

-- describir qué hace la función: Dada una red social y un usuario, devuelve una lista de los amigos de ese usuario.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined


cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
