
-- Completar con los datos del grupo
--
-- Nombre de Grupo: KSEA_team
-- Integrante 1: Kevin Ezequiel La Rocca, kevinlarocca2000@gmail.com, 874/23
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Angel Guillermo Reyes Vega, rvangelse@gmail.com, 252/23

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

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs

empiezaCon :: (Eq t) => [t] -> t
empiezaCon [] = error "La lista se encuentra vacía"
empiezaCon (x:xs) = x

terminaCon :: (Eq t) => [t] -> t
terminaCon [] = error "La lista se encuentra vacía"
terminaCon [x] = x
terminaCon (x:xs) = terminaCon xs

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs = False
                    | otherwise = sinRepetidos xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ = False 
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) = mismosElementos (x:xs) ys
                              | otherwise = False
                              
usuarioValido :: Usuario -> Bool --(1, "Kevin")
usuarioValido a | idDeUsuario a > 0 && nombreDeUsuario a /= "" = True
                | otherwise = False
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) = not (pertenece x xs) && noHayIdsRepetidos xs

usuariosValidos :: [Usuario] -> Bool --[(1, "Kevin"), (2, "Angel")etc...]
usuariosValidos [] = False
usuariosValidos (x:xs) | usuarioValido x && noHayIdsRepetidos (x:xs) = True
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
--1)
proyectarNombres :: [Usuario] -> [String] -- Dada una lista de usuarios, devuelvo los nombres.
proyectarNombres [] = []
proyectarNombres (x:xs) = nombreDeUsuario x:proyectarNombres xs

nombresDeUsuarios :: RedSocial -> [String] -- Dada una red social, devuelvo los nombres de los usuarios.
nombresDeUsuarios x = proyectarNombres(usuarios x)

-- 2)
listadeUsuarios :: [Relacion] -> Usuario -> [Usuario] --Dada una lista de relaciones y un usuario, devuelve la lista de usuarios que se relacionan con ese usuario.
listadeUsuarios [] _ = []
listadeUsuarios (x:xs) a | a == fst x = snd x: listadeUsuarios xs a
                         | a == snd x = fst x: listadeUsuarios xs a
                         | otherwise = listadeUsuarios xs a
amigosDe :: RedSocial -> Usuario -> [Usuario] --Dada una red social y un usuario, devuelve una lista de los amigos de ese usuario.
amigosDe x y = listadeUsuarios (relaciones x) y

--Ejm de Red Social: ([(1, "A"), (2, "K"), (3, "E"), (4, "S")], [((2, "K"), (1, "A")), ((2, "K"), (3,"E")), ((2, "K"), (4, "S"))], [((1, "K"), "hola", [(1, "A")])])

-- describir qué hace la función: .....
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
