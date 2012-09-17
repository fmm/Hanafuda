
module Util (
	listaRandomica
 ) where
 
import System.Random

f :: IO Int
f = randomIO

contem :: [Int] -> Int -> Bool
contem [] _ = False
contem (x:xs) y = x == y || contem xs y

gen :: [Int] -> IO [Int]
gen lista = do
 if(length lista >= 48) then do
  return lista
  else do
   novo <- f
   let opa = novo `mod`48
   if(contem lista opa) then do
     gen lista
     else do
     gen (lista ++ [opa])
     
listaRandomica = gen []
