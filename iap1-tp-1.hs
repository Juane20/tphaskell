-- Completar con los datos del grupo
--
-- Nombre de Grupo: KSEA_team
-- Integrante 1: Kevin Ezequiel La Rocca, kevinlarocca2000@gmail.com, 874/23
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre) Ej (1, "Kevin")     [1,2] [2,1]
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan Ej ((6, "Kevin"), (2, "Angel"), ((2, "Angel"), (6, "Kevin")) Esto solo contiene UN ELEMENTO
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
-- ((1, "Kevin"), "Casa", [(2, "Angel"), (3, "Sofia")])
type RedSocial = ([Usuario], [Relacion], [Publicacion])
-- ([(1, "Kevin"), (2, "Angel"), (3, "Sofia")], [])

--Ejemplos:
usuarioElias = (1, "Elias")
usuarioAngel = (2, "Angel")
usuarioKevin = (3, "Kevin")
usuarioSofia = (4, "Sofia")

usuariosRedA = [usuarioElias, usuarioAngel]
usuariosRedB = [usuarioKevin, usuarioSofia]

relacionE_A = (usuarioElias, usuarioAngel)
relacionE_K = (usuarioElias, usuarioKevin)
relacionE_S = (usuarioElias, usuarioSofia)
relacionA_K = (usuarioAngel, usuarioKevin)
relacionA_S = (usuarioAngel, usuarioSofia)
relacionK_S = (usuarioKevin, usuarioSofia)

relacionesRedA = [relacionE_A, relacionA_K, relacionK_S]
relacionesRedB = [relacionE_K, relacionE_S, relacionA_S]

publicacionE_1 = (usuarioElias, "Mi primera publicación.", [usuarioSofia, usuarioKevin])
publicacionE_2 = (usuarioElias, "Hola!", [usuarioSofia, usuarioAngel])
publicacionK_1 = (usuarioKevin, "Esta es la primera publicación!", [usuarioElias, usuarioSofia])
publicacionK_2 = (usuarioKevin, "Mi segunda publicación", [usuarioAngel])
publicacionA_1 = (usuarioAngel, "Soy Ángel y esta es mi primer publicación!", [usuarioKevin, usuarioElias])
publicacionA_2 = (usuarioAngel, "Hoooolaaaa, publicación n°2!", [])
publicacionS_1 = (usuarioSofia, "Soy Sofía!", [usuarioKevin])
publicacionS_2 = (usuarioSofia, "Qué buena red!", [usuarioAngel])

primeraRed = ([usuarioElias, usuarioKevin, usuarioAngel], [relacionE_K, relacionA_K], [publicacionK_2, publicacionA_1, publicacionA_2])
segundaRed = ([usuarioKevin, usuarioSofia, usuarioElias], [relacionE_S, relacionE_K, relacionK_S], [publicacionE_1, publicacionK_1, publicacionS_1])

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

-- Funciones auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool -- Pertenece el elemento de tipo t a la lista de elementos de tipo t
pertenece _ [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs

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
mismosElementos [] _ = False
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) = mismosElementos xs (quitar x (y:ys))
                              | otherwise = False
-- Ejercicios

--1)
proyectarNombres :: [Usuario] -> [String] -- Dada una lista de usuarios, devuelvo los nombres.
proyectarNombres [] = []
proyectarNombres (x:xs) = (nombreDeUsuario x:proyectarNombres xs)

nombresDeUsuarios :: RedSocial -> [String] -- Dada una red social, devuelvo los nombres de los usuarios.
nombresDeUsuarios x = proyectarNombres(usuarios(x))

--2)
listaDeUsuarios :: [Relacion] -> Usuario -> [Usuario] --Dada una lista De relaciones y un usuario, Devuelve la lista De usuarios que se relacionan con ese usuario.
listaDeUsuarios [] _ = []
listaDeUsuarios (x:xs) a | a == fst x = snd x: listaDeUsuarios xs a
                         | a == snd x = fst x: listaDeUsuarios xs a
                         | otherwise = listaDeUsuarios xs a
amigosDe :: RedSocial -> Usuario -> [Usuario] --Dada una red social y un usuario, devuelve una lista de los amigos de ese usuario.
amigosDe x y = listaDeUsuarios (relaciones x) y

--3)

longitud :: [t] -> Int --Dada una lista, me devuelve cuántos elementos tiene.
longitud [] = 0
longitud (x:xs) = (longitud xs) + 1

cantidadDeAmigos :: RedSocial -> Usuario -> Int --Dada una red social y un usuario, nos devuelve cuántos amigos tiene ese usuario en esa red.
cantidadDeAmigos rd us = longitud(amigosDe rd us)

--4)
{-maximo :: [Int] -> Int -- Dada una lista, me devuelve el valor más grande de esa lista.
maximo [x] = x
maximo (x:xs)
    | (maximo xs) > x = maximo xs
    | otherwise = x-}

usuarioConMasAmigos :: RedSocial -> Usuario --Dada una red social, me devuelve el usuario con más amigos.
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
listaDePublicaciones :: [Publicacion] -> Usuario -> [Publicacion] --Dada una lista de publicaciones y un usuario, devuelve la lista de publicaciones de ese usuario.
listaDePublicaciones [] _ = []
listaDePublicaciones (x:xs) us 
    | usuarioDePublicacion x == us = (x:listaDePublicaciones xs us)
    | otherwise = listaDePublicaciones xs us

publicacionesDe :: RedSocial -> Usuario -> [Publicacion] --Dada una red social y un usuario, devuelve la lista de publicaciones de ese usuario.
publicacionesDe rd us = listaDePublicaciones (publicaciones rd) us

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
