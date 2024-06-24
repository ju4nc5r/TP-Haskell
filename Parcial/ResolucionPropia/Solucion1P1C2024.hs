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


stockDeProducto :: [(String,Int)] -> String -> Int
stockDeProducto [] s = 0
stockDeProducto (x:xs) producto | producto == fst x = snd x 
                                | otherwise = stockDeProducto xs producto

-- Ejercicio 4

{-
problema aplicarOferta (stock:seq⟨seq⟨Char⟩ x Z⟩, precios: seq⟨seq⟨Char⟩ x R⟩: seq⟨seq⟨Char⟩ x R⟩ {
requiere: {No hay productos repetidos en stock}
requiere: {Todas las cantidades de los productos que hay en stock son mayores a cero}
requiere: {No hay productos repetidos en precios}
requiere: {Todos los precios de los productos son mayores a cero}
requiere: {Todo producto en stock aparece en la lista de precios}
asegura: {|res| = | precios|}
asegura: {Para todo 0 <= i < |precios| si stockDeProducto(stock, precios[i]0) > 10 entonces res[i]0 = precios[i]0 y
res[i]1 = precios[i]1 * 0,80}
asegura: {Para todo 0 <= i < |precios| si stockDeProducto(stock, precios[i]0) <= 10 entonces res[i]0 = precios[i]0 y
res[i]1 = precios[i]1}
}
-}


-- Para todo precio si stockDeProducto > 10 entonces precio del producto se actualiza.  

aplicarOferta :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
aplicarOferta [] [] = []
aplicarOferta (x:xs) (y:ys) | stockDeProducto (x:xs) (fst x) > 10 = (fst x, nuevoPrecio (snd y)) : aplicarOferta xs ys
                            | otherwise = (fst x, snd y) : aplicarOferta xs ys


-- 0.8 * 5 = 4 
nuevoPrecio :: Int -> Int
nuevoPrecio x = (x * 4) `div` 5