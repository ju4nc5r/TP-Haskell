{-
Ejercicio 1 (2 puntos)
problema aproboMasDeNMaterias (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩, alumno:seq⟨Char⟩, n: Z) : Bool {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {n > 0}
  requiere: {El alumno se encuentra en el registro }
  asegura: {res = true <=> el alumno tiene más de n notas de finales mayores o iguales a 4 en el registro}
}
-}

-- No tiene que haber, nombres repetidos en el registro. 
-- 0 < notas < 10
-- n > 0
-- ejemplo de uso "aproboMasdeNMaterias [(carlos, [4,5,6,8,2,4,6]]  

-- Necesito buscar el nombre dado en el registro y verificar si existen mas de n notas de dicho alumno mayores a 4. 
-- Ejemplo: [("juancruz",[2,5,7,3]),("antonella",[4,6,2,4])] "juancruz" 2
aproboMasDeNMaterias :: [(String, [Int])] -> String -> Int -> Bool
aproboMasDeNMaterias (x:xs) s n | s == fst x = cantMateriasAprobadas (snd x) n
                                | otherwise = aproboMasDeNMaterias xs s n


cantMateriasAprobadas :: [Int] -> Int -> Bool 
cantMateriasAprobadas [] n = False
cantMateriasAprobadas (x:xs) n | n == 0 = True
                               | x >= 4 = cantMateriasAprobadas (xs) (n - 1)
                               | otherwise = cantMateriasAprobadas xs n

{-
Ejercicio 2 (2 puntos)
problema buenosAlumnos (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨seq⟨Char⟩⟩ {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  asegura: {res es la lista de los nombres de los alumnos que están en registro cuyo promedio de notas es mayor o igual a 8 y no tiene aplazos (notas menores que 4)}
}
Para resolver el promedio pueden utilizar la función del Preludio de Haskell fromIntegral que dado un valor de tipo Int devuelve su equivalente de tipo Float.
-}

-- Devuelvo la lista de nombres de los alumnos con promedio >= 8 

-- Para hacerlo:
-- recorrer las tuplas y contar cuantas notas tiene dicho alumno. 
-- Obtener la sumatoria de cada nota por alumno. 
-- Calcular el promedio del alumno. 
-- Si promedio >= 8, lo agrego a la lista. Sino continuo con el siguiente promedio.  


buenosAlumnos :: [(String,[Int])] -> [String]
buenosAlumnos [] = []
buenosAlumnos (x:xs) | promedioAlumno (snd x) (sumatoriaNotas (snd x)) >= 8 && noTieneAplazos (snd x) = fst x : buenosAlumnos xs
                     | otherwise = buenosAlumnos xs

noTieneAplazos :: [Int] -> Bool 
noTieneAplazos [] = True
noTieneAplazos (x:xs) | x < 4 = False
                      | otherwise = noTieneAplazos xs

sumatoriaNotas :: [Int] -> Int
sumatoriaNotas [] = 0
sumatoriaNotas (x:xs) = x + sumatoriaNotas xs 

promedioAlumno :: [Int] -> Int -> Float
promedioAlumno (x:xs) n | length(x:xs) > 0 = fromIntegral n / fromIntegral (length (x:xs))


{-
Ejercicio 3 (2 puntos)
problema mejorPromedio (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨Char⟩ {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {|registro| > 0 }
  asegura: {res es el nombre del alumno cuyo promedio de notas es el más alto; si hay más de un alumno con el mismo promedio de notas, devuelve el nombre de alumno que aparece primero en registro}
}
-}

-- Si el promedio del alumno es mayor al anterior, entonces agrego el nombre a la lista. 

-- Utilizando la funcion promedioAlumno, puedo armar una lista de tuplas con el nombre del alumno y su promedio. 
-- Si el promedio del Alumno es mayor al anterior, entonces continuo desde este. 
-- Si el promedio del Alumno es menor al anterior, entonces agrego el anterior a la cola y saco actual. 

mejorPromedio :: [(String,[Int])] -> String
mejorPromedio [] = [] 
mejorPromedio (x:xs) = mejorPromedioAux (promediosAlumnos (x:xs))

mejorPromedioAux :: [(String,Float)] -> String
mejorPromedioAux (x:xs) | xs == [] = fst x
                        | snd x < snd (head xs) = mejorPromedioAux xs
                        | snd x >= snd (head xs) = mejorPromedioAux (x:tail(xs))
                        | otherwise = mejorPromedioAux xs

promediosAlumnos :: [(String,[Int])] -> [(String,Float)]
promediosAlumnos [] = []
promediosAlumnos (x:xs) = (fst x, promedioAlumno (snd x) (sumatoriaNotas (snd x))) : promediosAlumnos xs


{- 
Ejercicio 4 (3 puntos)
problema seGraduoConHonores (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩, cantidadDeMateriasDeLaCarrera: Z, alumno: seq⟨Char⟩ ) : Bool {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {cantidadDeMateriasDeLaCarrera > 0}
  requiere: {El alumno se encuentra en el registro }
  requiere: {|buenosAlumnos(registro)| > 0}
  asegura: {res <=> true si aproboMasDeNMaterias(registro, alumno, cantidadDeMateriasDeLaCarrera -1) = true 
  y alumno pertenece al conjunto de buenosAlumnos(registro) 
  y el promedio de notas de finales de alumno está a menos (estrictamente) de 1 punto del mejorPromedio(registro)}
}
-}

-- Para calcular la distancia al mejor promedio: 
-- Necesito conocer cual es el promedio del mejor promedio, para eso uso la funcion promediosAlumnos
-- Resto el mejor promedio respecto al prom actual. Si la diferencia es menor a 1 entonces devuelvo True. 



pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False 
pertenece t (x:xs) | x == t = True
                   | otherwise = pertenece t xs


seGraduoConHonores :: [(String,[Int])] -> Int -> String -> Bool
--seGraduoConHonores [] n s = False
seGraduoConHonores (x:xs) n s | aproboMasDeNMaterias (x:xs) s (n) && pertenece s (buenosAlumnos (x:xs)) && distanciaMejorProm (promediosAlumnos (x:xs)) s = True
--                              | not (aproboMasDeNMaterias (x:xs) s (n-1)) && not (pertenece s (buenosAlumnos (x:xs))) && not (distanciaMejorProm (promediosAlumnos (x:xs)) s) = False
--                              | otherwise = seGraduoConHonores xs n s
                              | otherwise = False

promedio :: [(String,Float)] -> String -> Float
promedio (x:xs) s | s == fst x = snd x
                  | otherwise = promedio xs s


distanciaMejorProm :: [(String,Float)] -> String -> Bool
distanciaMejorProm (x:xs) s | (promedio (x:xs) (mejorPromedioAux(x:xs)) - (promedio (x:xs) s)) < 1 = True
                            | otherwise = False
