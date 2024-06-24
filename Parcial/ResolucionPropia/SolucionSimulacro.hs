{- Ejercicio 1
problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
  requiere: {True}
  asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
}
1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.
-}

-- No contiene tuplas repetidas, ni ambas con componentes iguales. 

pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece elem [] = False
pertenece elem (x:xs) | elem == x = True
                      | otherwise = elem `pertenece` xs   

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas (x:xs) | xs == [] = True
                         | pertenece x xs = False
                         | fst x /= snd x = relacionesValidas xs
                         | otherwise = False

{-
Ejercicio 2
problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res no tiene elementos repetidos}
  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
}
-}

-- Requiere que sean relaciones validas
-- Devuelve una lista con los nombres de las personas involucrados en la relacion

personas :: [(String,String)] -> [String]
personas (x:xs) | relacionesValidas (x:xs) = eliminarRepetidos (personasRepetidas (x:xs))

personasRepetidas :: [(String,String)] -> [String]
personasRepetidas [] = []
personasRepetidas ((x1,x2):xs) = x1 : x2 : personasRepetidas xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x:eliminarRepetidos (quitarTodos x xs)

quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs
                     | otherwise = x:(quitarTodos e xs)


{-
Ejercicio 3
problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
}
-}

amigosDe :: String -> [(String,String)] -> [String]
amigosDe s [] = []
amigosDe s (x:xs) | s == snd x = fst x : amigosDe s (xs)
                  | s == fst x = snd x : amigosDe s (xs)
                  | otherwise = amigosDe s (xs)


{-
Ejercicio 4
problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
  requiere: {relaciones no vacía}
  requiere: {relacionesValidas(relaciones)}
  asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
}
-}

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos [] = []
personaConMasAmigos (x:xs) = mayorAparicion(cantAmigos (x:xs))


cantAmigos :: [(String,String)] -> [(String,Int)]
cantAmigos [] = []
cantAmigos (x:xs) = (fst x, length(amigosDe (fst x) (x:xs))) : cantAmigos (xs)


mayorAparicion :: [(String,Int)] -> String
mayorAparicion (x:xs) | xs == [] = fst x
                      | snd x < snd (head xs) = mayorAparicion xs  
                      | snd x > snd (head xs) = mayorAparicion (x:tail(xs))

-- Si el segundo es mayor que el primero, continuo con la cola de mi lista. 
-- Si el segundo es menor que el primero, continuo con la cabeza de mi cola. 