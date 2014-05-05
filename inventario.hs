module Main where

import System.Random
import Data.Char

main :: IO ()
main = exec 5000 printRow

exec :: Int -> (Int -> IO ()) -> IO ()
exec 0 _ = return ()
exec n action = do
	action n
	exec (n-1) action

mkRandomIntBetween :: Int -> Int -> IO Int
mkRandomIntBetween l h = getStdRandom $ randomR (l,h)

printRow :: Int -> IO ()
printRow row_number = do
	hasDiscount <- mkRandomHasDiscount

	color_id <- mkRandomColorID
	talla_id <- mkRandomTallaID
	marca_id <- mkRandomMarcaID
	prenda_id <- mkRandomPrendaID
	precio <- mkPrecio marca_id prenda_id

	discount <- mkRandomDiscount hasDiscount

	putStrLn $ "INSERT INTO inventario VALUES (" ++ (show row_number) ++ "," ++ (show color_id) ++ ","
			++ (show talla_id) ++ "," ++ (show marca_id) ++ "," ++ (show prenda_id) 
                        -- ++ (map toUpper $ show hasDiscount) ++
			++ "," ++ (show discount) ++ "," ++ (show precio) ++ ")"


-- recibe una marca, una prenda y retorna un precio
mkPrecio :: Int -> Int -> IO Int
-- old navy
mkPrecio 1 1 = mkRandomIntBetween 20 40
mkPrecio 1 3 = mkRandomIntBetween 20 40
mkPrecio 1 _ = mkRandomIntBetween 25 50
-- benetton
mkPrecio 2 1 = mkRandomIntBetween 40 60
mkPrecio 2 3 = mkRandomIntBetween 40 60
mkPrecio 2 _ = mkRandomIntBetween 50 80
-- lacoste
mkPrecio 3 1 = mkRandomIntBetween 60 80
mkPrecio 3 3 = mkRandomIntBetween 60 80
mkPrecio 3 _ = mkRandomIntBetween 80 130
-- aeropostale
mkPrecio 4 1 = mkRandomIntBetween 50 90
mkPrecio 4 2 = mkRandomIntBetween 50 90
mkPrecio 4 _ = mkRandomIntBetween 65 150

mkRandomColorID :: IO Int
mkRandomColorID = getStdRandom $ randomR (1,6)
mkRandomTallaID :: IO Int
mkRandomTallaID = getStdRandom $ randomR (1,7)
mkRandomMarcaID :: IO Int
mkRandomMarcaID = getStdRandom $ randomR (1,4)
mkRandomPrendaID :: IO Int
mkRandomPrendaID = getStdRandom $ randomR (1,5)

mkRandomHasDiscount :: IO Bool
mkRandomHasDiscount = (getStdRandom random) :: IO Bool

mkRandomDiscount :: Bool -> IO Double
mkRandomDiscount False = return $ 0.0
mkRandomDiscount True = do
	rnd_int <- getStdRandom (randomR (1,3))
	return $ rnd_int * (0.1)
