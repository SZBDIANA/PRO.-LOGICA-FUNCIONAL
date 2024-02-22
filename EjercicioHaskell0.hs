
sumarLista :: [Int] -> Int
sumarLista numeros = sum numeros

factorial :: Int -> Int 
factorial x
            | x == 0 = 1
            | otherwise = x * factorial (x-1)

numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

longitudCadena :: String -> Int
longitudCadena texto = length(texto)

reversoLista :: [a] -> [a]
reversoLista [] = []  
reversoLista (x:xs) = reversoLista xs ++ [x] 

duplicarElementos :: [Int] -> [Int]
duplicarElementos [] = [] 
duplicarElementos (x:xs) = (x * 2) : duplicarElementos xs 

filtrarPares :: [Int] -> [Int]
filtrarPares lista = [x | x <- lista, even x]

fibonacci :: Int -> Int
fibonacci numero
            | numero == 0 = 0
            | numero == 1 = 1
            | otherwise = fibonacci (numero-1) + fibonacci (numero-2)

divisores :: Int -> [Int]
divisores numero = [x | x <- [1..numero], numero `rem` x == 0]

esPalindromo :: String -> Bool
esPalindromo texto 
                |texto == reverse texto = True
                |otherwise = False 