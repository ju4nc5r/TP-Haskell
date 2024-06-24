module Solucion1erParcial where 


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
