-- Ejercicio 1 - Votos en Blanco

{-
problema votosEnBlanco(formulas : seq < String × String >, votos : seq < Z >, cantT otalV otos : Z) : Z{
requiere : {formulasValidas(formulas)}
requiere : {|formulas| = |votos|}
requiere : { Todos los elementos de votos son mayores o iguales que 0}
requiere : { La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
asegura : {res es la cantidad de votos emitidos que no correspondieron a niguna de las formulas que se presentaron }
}
-}

-- tengo que devolver la diferencia de tamaño entre las dos listas

recuentoDeVotosPositivos :: [Int] -> Int
recuentoDeVotosPositivos [] = 0
recuentoDeVotosPositivos (x:xs) = x + recuentoDeVotosPositivos xs

votosEnBlanco :: [(String,String)] -> [Int] -> Int -> Int
votosEnBlanco (x:xs) (y:ys) n = n - (recuentoDeVotosPositivos (y:ys))

-- Ejercicio 2 - Formulas Validas

{-
problema formulasValidas(formulas : seq < String × String >) : Bool{
requiere : {True}
asegura : {(res = true) ↔ formulas no contiene nombres repetidos, es decir que cada candidato esta en una unica formula
(no se puede ser candidato a presidente y a vicepresidente ni en la misma formula ni en formulas distintas) }
}
-}

pertenece :: (Eq t) => t -> [(t,t)] -> Bool
pertenece s [] = False
pertenece s (x:xs) | fst x == s || snd x == s = True
                   | otherwise = pertenece s xs 


formulasValidas :: [(String,String)] -> Bool
formulasValidas [] = True
formulasValidas (x:xs) | pertenece (fst x) xs || pertenece (snd x) xs = False
                       | otherwise = formulasValidas xs


-- Ejercicio 3 - Porcentaje de Votos

{-
problema porcentajeDeVotos(presidente : String, formulas : seq < String × String >, votos : seq < Z >) : R{
requiere : {La primera componente de algun elemento de formulas es presidente}
requiere : {formulasValidas(formulas)}
requiere : {|formulas| = |votos|}
requiere : { Todos los elementos de votos son mayores o iguales que 0}
requiere : { Hay al menos un elemento de votos que es mayor estricto que 0}
asegura : {res es el porcentaje de votos que obtuvo la formula encabezada por presidente sobre el total de votos afirmativos }
}
-}

-- Devolver el porcentaje de votos segun un presidente.

-- Necesito: 
-- Conocer cual es la cantidad de votos totales (sumatoria de todos los elementos de la lista votos)
-- Dividir la cantidad de votos del presidente / votos totales


-- Ejemplo de uso: porcentajeDeVotos "milei" [("milei","villaruel"),("cristina","grabois"),("alberto","kiciloff")] [50,40,10]

porcentajeDeVotos :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeVotos s (x:xs) (y:ys) = fromIntegral ((votosPresidente s (x:xs) (y:ys)) * 100) / fromIntegral (recuentoDeVotosPositivos (y:ys)) 

votosPresidente :: String -> [(String,String)] -> [Int] -> Int
votosPresidente s (x:xs) (y:ys) | s == fst x = y
                                | otherwise = votosPresidente s xs ys


-- Ejercicio 4 - Proximo Presidente
{-
problema proximoPresidente(formulas : seq < String × String >, votos : seq < Z >) : String{
requiere : {La primera componente de algun elemento de formulas es presidente}
requiere : {formulasValidas(formulas)}
requiere : {|formulas| = |votos|}
requiere : { Todos los elementos de votos son mayores o iguales que 0}
requiere : { Hay al menos un elemento de votos que es mayor estricto que 0}
requiere : {|formulas| > 0}
asegura : {res es el candidato a presidente de formulas mas votado de acuerdo a los votos contabilizados en votos}
}
-}

-- Necesito tomar el nombre del presidente y verificar cual es su voto. (votosPresidente)
-- 


proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente (x:xs) (y:ys) = presidenteMasVotado (presidentesConMasVotos (x:xs) (y:ys))

presidenteMasVotado :: [String] -> String
presidenteMasVotado (x:xs) | xs == [] = x
                            | otherwise = presidenteMasVotado xs

presidentesConMasVotos :: [(String,String)] -> [Int] -> [String]
presidentesConMasVotos (x:xs) (y:ys) | xs == [] || ys == [] = []
                                     | votosPresidente (fst x) (x:xs) (y:ys) > votosPresidente (fst (head xs)) (x:xs) (y:ys) = fst x : presidentesConMasVotos (xs) (ys)
                                     | votosPresidente (fst x) (x:xs) (y:ys) < votosPresidente (fst (head xs)) (x:xs) (y:ys) = fst (head xs) : presidentesConMasVotos (xs) (ys)
                                     | otherwise = presidentesConMasVotos xs ys


{-
proximoPresidente :: [(String, String)] -> [Int] -> String
proximoPresidente [(presi,_)] _ = presi
proximoPresidente (f1:f2:fs) (v1:v2:vs) | v1 >= v2 = proximoPresidente (f1:fs) (v1:vs)
                                        | otherwise = proximoPresidente (f2:fs) (v2:vs)
-}                                        