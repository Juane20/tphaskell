module Solucion where
-- Nombre de Grupo: KSEA_team
-- Integrante 1: Kevin Ezequiel La Rocca, kevinlarocca2000@gmail.com, 874/23
-- Integrante 2: Juan Elias Cabrera, cabreraelias182@gmail.com, 501/23
-- Integrante 3: Sofia Nur Copaga Vargas, copagavargassn@gmail.com, 238/23
-- Integrante 4: Angel Guillermo Reyes Vega, rvangelse@gmail.com, 252/23
type Usuario = (Integer, String)
type Relacion = (Usuario, Usuario)
type Publicacion = (Usuario, String, [Usuario])
type RedSocial = ([Usuario], [Relacion], [Publicacion])
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
--Usadas en el ejercicio 1--
proyectarNombres :: [Usuario] -> [String] -- Dada una lista de usuarios, devuelvo los nombres.
proyectarNombres [] = []
proyectarNombres (x:xs) = eliminarRepetidos((nombreDeUsuario x:proyectarNombres xs))

eliminarRepetidos :: (Eq t) => [t] -> [t] --Dada una lista, quita los repetidos.
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (y:ys)
    | pertenece y ys = y:eliminarRepetidos(quitarTodos y ys)
    | otherwise = (y:ys) 
--Usadas en el ejercicio 2
listaDeUsuarios :: [Relacion] -> Usuario -> [Usuario] --Dada una lista De relaciones y un usuario, Devuelve la lista De usuarios que se relacionan con ese usuario.
listaDeUsuarios [] _ = []
listaDeUsuarios (x:xs) us 
    | us == fst x = snd x: listaDeUsuarios xs us
    | us == snd x = fst x: listaDeUsuarios xs us
    | otherwise = listaDeUsuarios xs us

--Usadas en el ejercicio 3
longitud :: [t] -> Int --Dada una lista, me devuelve cuántos elementos tiene.
longitud [] = 0
longitud (x:xs) = (longitud xs) + 1
--Usadas en el ejercicio 4
auxUsuarioConMasAmigos :: RedSocial -> [Usuario] -> Usuario --Dada una red social y una lista de usuarios, devuelve el que más amigos tiene.
auxUsuarioConMasAmigos rd [x] = x
auxUsuarioConMasAmigos rd (x:xs)
    | cantidadDeAmigos rd (auxUsuarioConMasAmigos rd xs) > cantidadDeAmigos rd x = auxUsuarioConMasAmigos rd xs
    | otherwise = x
--Usadas en el ejercicio 5
chequearCantidadAmigos :: RedSocial -> [Usuario] -> Bool --Dada una red social y una lista de usuarios, devuelve True si y solo si, al menos un usuario tiene más de un millón de amigos.   
chequearCantidadAmigos _ [] = False 
chequearCantidadAmigos rd (x:xs) 
    | (cantidadDeAmigos rd x) > 10 = True 
    | otherwise = chequearCantidadAmigos rd xs
--Usadas en el ejercicio 6--
listaDePublicaciones :: [Publicacion] -> Usuario -> [Publicacion] --Dada una lista de publicaciones y un usuario, devuelve la lista de publicaciones de ese usuario.
listaDePublicaciones [] _ = []
listaDePublicaciones (x:xs) us 
    | usuarioDePublicacion x == us = (x:listaDePublicaciones xs us)
    | otherwise = listaDePublicaciones xs us
--Usadas en el ejercicio 7--
chequearListaPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
chequearListaPublicaciones [] _ = []
chequearListaPublicaciones (x:xs) us 
    | pertenece us (likesDePublicacion x) = x:chequearListaPublicaciones xs us
    | otherwise = chequearListaPublicaciones xs us
--Usadas en el ejercicio 8--
mismosElementos :: (Eq t) => [t] -> [t] -> Bool -- Comprueba que dos listas tengan los mismos elementos (sin repetidos)
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ = False
mismosElementos (x:xs) (y:ys) 
    | pertenece x (y:ys) = mismosElementos xs (quitar x (y:ys))
    | otherwise = False
--Usadas en el ejercicio 9
cantPublicacionesDe :: RedSocial -> Usuario -> Int -- Dada una red social y un usuario, me devuelve la cantidad de publicaciones de ese usuario en la red social.
cantPublicacionesDe rd us = longitud(publicacionesDe rd us)

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
--Usadas en el ejercicio 10 y funciones que se utilizan en varias funciones.
cadenaDeAmigos :: [Relacion] -> Usuario -> [Usuario] -- Dada una lista de relaciones y un usuario, crea una cadena de amigos (Lista de Usuarios), que empieza por el amigo del amigo del us1
cadenaDeAmigos [] _ = []
cadenaDeAmigos (rel:rels) us1 
    | us1 == fst rel = listaDeUsuarios rels (snd rel) ++ cadenaDeAmigos rels (snd rel) 
    | us1 == snd rel = listaDeUsuarios rels (fst rel) ++ cadenaDeAmigos rels (snd rel)
    | otherwise = fst rel : snd rel : cadenaDeAmigos rels us1

finDeLaCadena :: [Usuario] -> Usuario -> Bool --Dada una cadena de Amigos, verifica que el us2 pertenezca a esta
finDeLaCadena listUs us2 = pertenece us2 listUs

pertenece :: (Eq t) => t -> [t] -> Bool -- Pertenece el elemento de tipo t a la lista de elementos de tipo t
pertenece _ [] = False
pertenece a (x:xs) 
    | a == x = True
    | otherwise = pertenece a xs

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
-- Ejercicios
--1)
nombresDeUsuarios :: RedSocial -> [String] -- Dada una red social, devuelvo los nombres de los usuarios.
nombresDeUsuarios rd = proyectarNombres(usuarios rd)
--2)
amigosDe :: RedSocial -> Usuario -> [Usuario] --Dada una red social y un usuario, devuelve una lista de los amigos de ese usuario.
amigosDe us listUs = listaDeUsuarios (relaciones us) listUs
--3)
cantidadDeAmigos :: RedSocial -> Usuario -> Int --Dada una red social y un usuario, nos devuelve cuántos amigos tiene ese usuario en esa red.
cantidadDeAmigos rd us = longitud(amigosDe rd us)
--4)
usuarioConMasAmigos :: RedSocial -> Usuario --Dada una red social, me devuelve el usuario con más amigos.
usuarioConMasAmigos rd = auxUsuarioConMasAmigos rd (usuarios rd)
--5)
estaRobertoCarlos :: RedSocial -> Bool --5)Función que describe si EXISTE algún elemento dentro de la lista de Usuarios tal que tenga mas de 10 de amigos.
estaRobertoCarlos rd = chequearCantidadAmigos rd (usuarios rd)
--6)
publicacionesDe :: RedSocial -> Usuario -> [Publicacion] --Dada una red social y un usuario, devuelve la lista de publicaciones de ese usuario.
publicacionesDe rd us = listaDePublicaciones (publicaciones rd) us
--7)
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion] --Dada una red social y un usuario, devuelve las publicaciones a las que le dio like ese usuario.
publicacionesQueLeGustanA rd us = chequearListaPublicaciones (publicaciones rd) us
--8)
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool --Dada una red social y dos usuarios, devuelve true si a ambos usuarios les gustan las mismas publicaciones.
lesGustanLasMismasPublicaciones rd us1 us2 = mismosElementos(publicacionesQueLeGustanA rd us1) (publicacionesQueLeGustanA rd us2)
--9)
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool --Dada una red social y un usuario, evalúa si, en esa red social, hay un usuario que le dio like a todas las publicaciones del usuario dado, si es así, devuelve True.
tieneUnSeguidorFiel rd us = auxTieneUnSeguidorFiel rd us (likesPrimeraPublicacion rd us)
--10)
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rd us1 us2 
    | (cantidadDeAmigos rd us1 == 0 || cantidadDeAmigos rd us2 == 0) = False
    | pertenece us1 (amigosDe rd (us2)) = True
    | otherwise = finDeLaCadena (cadenaDeAmigos (relaciones rd) us1) us2
