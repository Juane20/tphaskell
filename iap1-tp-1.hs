-- Completar con los datos del grupo
--
-- Nombre de Grupo: KSEA_team
-- Integrante 1: Kevin Ezequiel La Rocca, kevinlarocca2000@gmail.com, 874/23
-- Integrante 2: Juan Elias Cabrera, cabreraelias182@gmail.com, 501/23
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

usuariosRedA = [usuarioElias, usuarioAngel, usuarioKevin]
usuariosRedB = [usuarioKevin, usuarioSofia, usuarioElias, usuarioAngel]

relacionE_A = (usuarioElias, usuarioAngel)
relacionE_K = (usuarioElias, usuarioKevin)
relacionE_S = (usuarioElias, usuarioSofia)
relacionA_K = (usuarioAngel, usuarioKevin)
relacionA_S = (usuarioAngel, usuarioSofia)
relacionK_S = (usuarioKevin, usuarioSofia)

relacionesRedA = [relacionE_A, relacionA_K, relacionK_S]
relacionesRedB = [relacionE_K, relacionE_S, relacionA_S]

publicacionE_1 = (usuarioElias, "Mi primera publicacion.", [usuarioSofia, usuarioKevin])
publicacionE_2 = (usuarioElias, "Hola!", [usuarioKevin, usuarioAngel])
publicacionK_1 = (usuarioKevin, "Esta es la primera publicacion!", [usuarioElias, usuarioSofia])
publicacionK_2 = (usuarioKevin, "Mi segunda publicacion", [usuarioAngel, usuarioKevin])
publicacionA_1 = (usuarioAngel, "Soy Angel y esta es mi primer publicacion!", [usuarioAngel, usuarioKevin, usuarioElias])
publicacionA_2 = (usuarioAngel, "Hoooolaaaa, publicacion n°2!", [usuarioAngel, usuarioKevin])
publicacionS_1 = (usuarioSofia, "Soy Sofia!", [usuarioKevin])
publicacionS_2 = (usuarioSofia, "Que buena red!", [usuarioAngel])

primeraRed = ([usuarioElias, usuarioKevin, usuarioAngel], [relacionE_K, relacionA_K], [publicacionK_2, publicacionA_1, publicacionA_2, publicacionE_1])
segundaRed = ([usuarioKevin, usuarioSofia, usuarioElias], [relacionE_S, relacionA_S, relacionK_S], [publicacionE_1, publicacionE_2, publicacionK_1, publicacionS_1])

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

quitarTodos :: (Eq t) => t -> [t] -> [t] --Dado un elemento y una lista, quita todas las apariciones de ese elemento en la lista.
quitarTodos t [] = []
quitarTodos t x
    | not (pertenece t x) = x
    | otherwise = quitarTodos t (quitar t x)

eliminarRepetidos :: (Eq t) => [t] -> [t] --Dada una lista, quita los repetidos.
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (y:ys)
    | pertenece y ys = y:eliminarRepetidos(quitarTodos y ys)
    | otherwise = (y:ys) 

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
nombresDeUsuarios rd = proyectarNombres(usuarios(rd))

--2)
listaDeUsuarios :: [Relacion] -> Usuario -> [Usuario] --Dada una lista De relaciones y un usuario, Devuelve la lista De usuarios que se relacionan con ese usuario.
listaDeUsuarios [] _ = []
listaDeUsuarios (x:xs) us 
    | us == fst x = snd x: listaDeUsuarios xs us
    | us == snd x = fst x: listaDeUsuarios xs us
    | otherwise = listaDeUsuarios xs us
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

auxUsuarioConMasAmigos :: RedSocial -> [Usuario] -> Usuario --Dada una red social y una lista de usuarios, devuelve el que más amigos tiene.
auxUsuarioConMasAmigos rd [x] = x
auxUsuarioConMasAmigos rd (x:xs)
    | cantidadDeAmigos rd (auxUsuarioConMasAmigos rd xs) > cantidadDeAmigos rd x = auxUsuarioConMasAmigos rd xs
    | otherwise = x

usuarioConMasAmigos :: RedSocial -> Usuario --Dada una red social, me devuelve el usuario con más amigos.
usuarioConMasAmigos rd = auxUsuarioConMasAmigos rd (usuarios rd)

--5)Función que describe si EXISTE algún elemento dentro de la lista de Usuarios tal que tenga mas de un millón de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rd = chequearCantidadAmigos rd (usuarios rd)

chequearCantidadAmigos :: RedSocial -> [Usuario] -> Bool --Dada una red social y una lista de usuarios, devuelve True si y solo si, al menos un usuario tiene más de un millón de amigos.   
chequearCantidadAmigos _ [] = False 
chequearCantidadAmigos rd (x:xs) 
    | (cantidadDeAmigos rd x) > 1000000 = True 
    | otherwise = chequearCantidadAmigos rd xs
--6)
listaDePublicaciones :: [Publicacion] -> Usuario -> [Publicacion] --Dada una lista de publicaciones y un usuario, devuelve la lista de publicaciones de ese usuario.
listaDePublicaciones [] _ = []
listaDePublicaciones (x:xs) us 
    | usuarioDePublicacion x == us = (x:listaDePublicaciones xs us)
    | otherwise = listaDePublicaciones xs us

publicacionesDe :: RedSocial -> Usuario -> [Publicacion] --Dada una red social y un usuario, devuelve la lista de publicaciones de ese usuario.
publicacionesDe rd us = listaDePublicaciones (publicaciones rd) us
--7)
--Funcion auxiliar que por cada elemento de nuestra lista de publicación, verifica si al usuario le gusta. Si le gusta, se lo agrega a la lista que se devolverá como resultado, si no hace recursión sin agregar la publicación actual.
chequearListaPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
chequearListaPublicaciones [] _ = []
chequearListaPublicaciones (x:xs) us 
    | pertenece us (likesDePublicacion x) = x:chequearListaPublicaciones xs us
    | otherwise = chequearListaPublicaciones xs us

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rd us = chequearListaPublicaciones (publicaciones rd) us
--8)
--Dada una red social y dos usuarios, devuelve true si a ambos usuarios les gustan las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rd us1 us2 = mismosElementos(publicacionesQueLeGustanA rd us1) (publicacionesQueLeGustanA rd us2)

-- Dada una red social y un usuario, devuelve True si hay un usuario que le dio like a todas las publicaciones del usuario de entrada.
cantPublicacionesDe :: RedSocial -> Usuario -> Int -- Dada una red social y un usuario, me devuelve la cantidad de publicaciones de ese usuario en la red social.
cantPublicacionesDe rd us = longitud(publicacionesDe rd us)
--9)
cantApariciones :: (Eq t) => t -> [t] -> Int --Dado un elemento y una lista, me dice cuántas veces aparece ese elemento en la lista.
cantApariciones _ [] = 0
cantApariciones x (y:ys)
    | x == y = 1 + cantApariciones x ys
    | otherwise = cantApariciones x ys

likesDeTodasLasPublicaciones :: [Publicacion] -> [Usuario] --Dada una lista de publicaciones, concatena los likes de éstas.
likesDeTodasLasPublicaciones [] = []
likesDeTodasLasPublicaciones (x:xs) = (eliminarRepetidos(likesDePublicacion x)++likesDeTodasLasPublicaciones xs)

likesPrimeraPublicacion :: RedSocial -> Usuario -> [Usuario] --Dado un usuario, devuelve los likes de la primera publicación de éste en la red social dada.
likesPrimeraPublicacion rd us = likesDePublicacion(head(publicacionesDe rd us))

auxTieneUnSeguidorFiel :: RedSocial -> Usuario -> [Usuario] -> Bool --Auxiliar que hace recursividad sobre los likes de la primera publicación del usuario de tieneUnSeguidorFiel.
auxTieneUnSeguidorFiel _ _ [] = False
auxTieneUnSeguidorFiel rd us (x:xs)
    | cantApariciones x (likesDeTodasLasPublicaciones(publicacionesDe rd us)) == cantPublicacionesDe rd us = True
    | otherwise = auxTieneUnSeguidorFiel rd us xs

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool --Dada una red social y un usuario, evalúa si, en esa red social, hay un usuario que le dio like a todas las publicaciones del usuario dado, si es así, devuelve True.
tieneUnSeguidorFiel rd us = auxTieneUnSeguidorFiel rd us (likesPrimeraPublicacion rd us)

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
