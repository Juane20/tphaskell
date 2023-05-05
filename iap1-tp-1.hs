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

--PREGUNTAR EN CLASE EL CASO [1,2,3] [1,2,3,3]
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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

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
