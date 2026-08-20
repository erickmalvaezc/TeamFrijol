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
  | n < 1 = "Frio Extremo"
  | n <= 15 = "Frio"
  | n < 25 = "Templado"
  | n <= 35 = "Calor"
  | otherwise = "Calor Extremo"
-- intercala :: a -> [a] -> [a]

-- data Expr
--   = Lit Int
--   | Suma Expr Expr
--   | Producto Expr Expr
--   deriving (Eq, Show)

-- evalua :: Expr -> Int
