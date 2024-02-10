--Ejercicio 1
aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio descuento = precio - ((precio/100)*descuento)
aplicarIva :: Float -> Float -> Float 
aplicarIva precio iva = precio + ((precio/100)*iva)

funcionFinal :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
funcionFinal preciosPorcentajes funcionAplicadora = sum $ map (\(precio, porcentaje) -> funcionAplicadora precio porcentaje) preciosPorcentajes

-- Ejercicio 2 Función que aplica una función a todos los elementos de una lista
aplicaFuncionLista :: (a -> b) -> [a] -> [b]
aplicaFuncionLista _ [] = []
aplicaFuncionLista f (x:xs) = f x : aplicaFuncionLista f xs
-- Función que calcula el cuadrado de un número
cuadrado :: Num a => a -> a
cuadrado n = n * n

-- Ejercicio 3 Función que recibe una frase y devuelve un diccionario con las palabras que contiene y su longitud
largoPalabra :: String -> [(String, Int)]
largoPalabra sentence = zip (words sentence) (map length (words sentence))

---- Ejercicio 4 Función que clasifica una nota en una calificación según los rangos especificados
clasificarCalificacion :: Double -> String
clasificarCalificacion score
    | score >= 95   = "Excelente"
    | score >= 85   = "Notable"
    | score >= 75   = "Bueno"
    | score >= 70   = "Suficiente"
    | otherwise     = "Desempeño insuficiente"

-- Función que convierte una cadena a mayúsculas sin importar el módulo
toUpper' :: String -> String
toUpper' = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A') else c)

-- Función que recibe un diccionario con las asignaturas y las notas de un alumno y devuelve otro diccionario con las asignaturas en mayúsculas y las calificaciones correspondientes
clasificaCali :: [(String, Double)] -> [(String, String)]
clasificaCali grades = zip (map (toUpper' . fst) grades) (map (clasificarCalificacion . snd) grades)


-- Ejercicio 5 
moduleVector :: [Float] -> Float
moduleVector v = sqrt (sum [x * x | x <- v])

-- Ejercicio 6 Función que devuelve una función que verifica si un valor es atípico en una muestra
calcularAtipicos :: [Double] -> [Double]
calcularAtipicos muestra = filter (\x -> abs ((x - media muestra) / desviacionEstandar muestra) > 3) muestra
  where
    media xs = sum xs / fromIntegral (length xs)
    desviacionEstandar xs = sqrt $ sum [(x - media xs)^2 | x <- xs] / fromIntegral (length xs)





