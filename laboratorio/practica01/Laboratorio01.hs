module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x*x + y*y)



sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares [] = 0
sumaCuadradosPares (x:xs)
  | even x   = x^2 + sumaCuadradosPares xs
  | otherwise = sumaCuadradosPares xs



aplicaTresVeces :: (a -> a) -> a -> a  
aplicaTresVeces f x = f (f (f x))  



-- varianza2 :: Double -> Double -> Double



clasificaTemperatura :: Int -> String
clasificaTemperatura n
  | n < 1 = "frio extremo"
  | n <= 15 = "frio"
  | n <= 25 = "templado"
  | n <= 35 = "calido"
  | otherwise = "calor extremo"

  
intercala :: a -> [a] -> [a]
intercala x [] = []
intercala y (x:xs) = x : auxi1 y xs

auxi1 :: a -> [a] -> [a]
auxi1 x [] = []
auxi1 y (x:xs) = y : x : auxi1 y xs


data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma x y) = evalua x + evalua y
evalua (Producto x y) = evalua x * evalua y

