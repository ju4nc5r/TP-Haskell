--Ej1
pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece elem [] = False
pertenece elem (x:xs) | elem == x = True
                      | otherwise = elem `pertenece` xs     

esMinuscula :: Char -> Bool
esMinuscula c | pertenece c ['a'..'z'] = True
              | otherwise = False

--Ej2
posicion :: Char -> [Char] -> Int
posicion c [] = 0 
posicion c (x:xs) | pertenece c (xs) && c /= x = posicion c (xs) + 1
                  | otherwise = posicion c xs

letraANatural :: Char -> Int
letraANatural c | esMinuscula c = posicion c ['a'..'z']

--Ej3 
traerLetra :: Int -> [Char] -> Char
traerLetra n (x:xs) | mod n 26 == 0 = x
                    | n < 26 = traerLetra (n-1) (xs) 
                    | otherwise = traerLetra (mod n 26) (x:xs)

desplazar :: Char -> Int -> Char
desplazar c n | esMinuscula c = traerLetra (posicion c ['a'..'z'] + n) ['a'..'z']
              | otherwise = c

--Ej4

cifrar :: [Char] -> Int -> [Char]
cifrar (x:xs) n | length (xs) == 0 = desplazar x n : []
                | length (xs) > 0 = desplazar x n : cifrar xs n

--Ej5

desplazarAtras :: Char -> Int -> Char
desplazarAtras c n | esMinuscula c = traerLetra (posicion c ['a'..'z'] - n) ['a'..'z']
                   | otherwise = c 

descifrar :: [Char] -> Int -> [Char]
descifrar (x:xs) n | length (xs) == 0 = desplazarAtras x n : []
                   | length (xs) > 0 = desplazarAtras x n : descifrar xs n

--Ej6
--cifrarLista :: [[Char]] -> [[Char]]
--cifrarLista (x:xs) | [] = [] 
--                   | [x] = cifrar [x] 4
--                   | otherwise = cifrarLista xs

--ejemplo:
--entrada: ls = ["compu", "labo", "intro"]
--res: ["compu", "mbcp", "kpvtq"]
