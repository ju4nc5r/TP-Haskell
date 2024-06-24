module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1

esMinuscula :: Char -> Bool
esMinuscula a = ord a >= ord 'a' && ord a <= ord 'z'

-- EJ 2

letraANatural :: Char -> Int
letraANatural a |  esMinuscula a = ord a - ord 'a'  

naturalALetra :: Int -> Char
naturalALetra n |  (n >= 0) && (n <= 25) = chr (n + 97)

-- EJ 3

desplazar :: Char -> Int -> Char
desplazar a n | esMinuscula a == False = a
              | ( ((letraANatural a)  + n) >= 0 ) && ( ((letraANatural a)  + n) <= 25 ) = naturalALetra ((letraANatural a) + n)
              | otherwise =  naturalALetra  (mod  ((letraANatural a) + n)  26)


-- EJ 4

cifrar :: String -> Int -> String
cifrar [] n = []
cifrar (m:ms)  n |  esMinuscula m == False = m : cifrar ms n
                 |  otherwise = (desplazar m n) : cifrar ms n 

-- EJ 5

descifrar :: String -> Int -> String
descifrar  [] n = []
descifrar (c:cs) n | esMinuscula c == False = c : descifrar cs n
                   | otherwise = desplazar c (-n) : descifrar cs n


-- EJ 6

cifrarLista :: [String] -> [String]
cifrarLista  (x:y:xs) = cifrarListaDesde (x:y:xs)  0 
    
cifrarListaDesde :: [String] -> Int -> [String]
cifrarListaDesde [] _ = []
cifrarListaDesde (x:xs) n = cifrar x  n : cifrarListaDesde xs (n+1)

-- EJ 7

frecuencia :: String -> [Float]
frecuencia (x:xs) = aux (x:xs) 0 (length (x:xs))
 

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) |  x == head xs = eliminarRepetidos xs
                         | otherwise = [x] ++ eliminarRepetidos (quitarTodos x (x:xs)) 
                         
aux :: String -> Int -> Int -> [Float]
aux (x:xs) n len | n == 26 = []
                 | elem (naturalALetra n) (eliminarRepetidos(x:xs)) == False = 0 : aux (x:xs) (n+1) len
                 | elem (naturalALetra n) (eliminarRepetidos(x:xs)) == True = porcentaje (division (cantidadDeApariciones (naturalALetra n) (x:xs)) len) : aux (x:xs) (n+1) len
                 
cantidadDeApariciones :: Char -> String -> Int
cantidadDeApariciones  _   [] = 0
cantidadDeApariciones  a  (l:ls)  | a == l  = 1 + cantidadDeApariciones a ls
                                  | otherwise = cantidadDeApariciones a ls    
--    [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]    
         
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                     | otherwise =  x : quitarTodos n xs 

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

porcentaje :: Float -> Float
porcentaje n = n * 100


-- Ej 8

cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente (x:xs) n = coso (aux2 (x:xs) (length(x:xs))) n  


aux2 :: String -> Int -> [(Char, Float)]
aux2 (x:xs) len | xs == [] = [(x, porcentaje (division (cantidadDeApariciones(x) (x:xs)) len))]
                | otherwise = (x, porcentaje (division (cantidadDeApariciones(x) (x:xs)) len)) : aux2 (quitarTodos x xs) len  

--necesito maximo de la parte y de la lista
coso :: [(Char, Float)] -> Int -> (Char, Float)
coso [] n = ('f', 0)
coso ((x,y):xs) n | length(xs) == 0 = ((desplazar x n), y) 
                  | y > snd (head(xs)) = coso ((x,y):(tail(xs))) n
                  | otherwise = coso (xs) n

-- EJ 9

esDescifrado :: String -> String -> Bool
esDescifrado s1 s2 = aux3 s1 s2 25

aux3 :: String -> String -> Int -> Bool
aux3 s1 s2 n | n < 0 = False
             | cifrar s1 n == s2 = True
             | otherwise = aux3 s1 s2 (n-1)
 
-- EJ 10

todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados _ = [("compu", "frpsx"), ("frpsx", "compu")]






-- EJ 11

expandirClave :: String -> Int -> String
expandirClave (x:xs) n | n == 1 = [x]
                       | n <= length (x:xs) = x : expandirClave (xs) (n-1)
                       | n > length (x:xs) = (x:xs) ++ expandirClave (x:xs) (n - (length(x:xs)))  


--Por ejemplo, para cifrar el mensaje “introduccion a
--la programacion” con la clave “compu”, primero debe expandirse la clave a la longitud del mensaje, y luego desplazar cada
--letra del mensaje original por la letra de la clave correspondiente:

-- EJ 12

cifrarVigenere :: String -> String -> String
cifrarVigenere (x:xs) (y:ys) | length (x:xs) == 0 = []
                             | length (x:xs) > 0 = expandirClave (y:ys) (length (x:xs)) 



-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere _ _ = "computacion"



-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
