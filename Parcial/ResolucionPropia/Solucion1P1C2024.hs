-- Ejercicio 1 
{-
problema generarStock (productos: seq⟨seq⟨Char⟩⟩): seq⟨seq⟨Char⟩ x Z⟩ {
requiere: {True}
asegura: {La longitud de res es igual a la cantidad de productos distintos que hay en productos}
asegura: {Para cada producto que pertenece a productos existe un i tal que 0 <= i < |res| y
res[i]0=producto y res[i]1 es igual a la cantidad de veces que aparece producto en productos}
}
-}

{-
pertenece :: String -> [(String,Int)] -> Bool
pertenece s [] = False
pertenece s (x:xs) | fst x == s = True
                   | otherwise = pertenece s xs 


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False
pertenece t (x:xs) | t == x = True
                   | otherwise = pertenece t xs 
-}

cantApariciones :: String -> [String] -> Int
cantApariciones _ [] = 0
cantApariciones s (x:xs) | s == x = cantApariciones s xs + 1
                         | otherwise = cantApariciones s xs  

quitarApariciones :: String -> [String] -> [String]
quitarApariciones s [] = [] 
quitarApariciones s (x:xs) | s == x = quitarApariciones s xs
                           | otherwise = x : quitarApariciones s xs

generarStock :: [String] -> [(String, Int)]
generarStock [] = []
generarStock (x:xs) = (x, cantApariciones x (x:xs)) : generarStock (quitarApariciones x xs)

-- Ejercicio 2
{-
problema stockDeProducto (stock:seq⟨seq⟨Char⟩ x Z⟩, producto: seq⟨Char⟩) : Z {
requiere: {No hay productos repetidos en stock}
requiere: {Todas las cantidades de los productos que hay en stock son mayores a cero}
asegura: {(res = 0 y producto no se encuentra en el stock) o (existe un i tal que 0 <= i < |stock| y producto=stock[i]0
y res = stock[i]1)}
}
-}

pertenece :: String -> [(String,Int)] -> Bool
pertenece s [] = False
pertenece s (x:xs) | fst x == s = True
                   | otherwise = pertenece s xs 

--producto :: String -> [(String,Int)] -> (String,Int)
--producto s (x:xs) | s == fst x = 

stockDeProducto :: [(String,Int)] -> String -> Int
stockDeProducto [] s = 0
stockDeProducto (x:xs) producto | producto == fst x = snd x 
                                | otherwise = stockDeProducto xs producto

