import qualified Data.ByteString.Lazy as BL
import Data.Binary

import Data.List (foldl')
import Data.Bits (shiftL, (.|.))
import System(getArgs)

data Bit = Z | O deriving (Show, Eq)

b2b :: Bit -> Word8
b2b Z = 0
b2b O = 1

packInList :: [Bit] -> [Word8]
packInList [] = []
packInList (b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : rest) = 
    ((b2b b1 `shiftL` 7) .|. (b2b b2 `shiftL` 6) .|. (b2b b3 `shiftL` 5) .|. (b2b b4 `shiftL` 4) .|.
     (b2b b5 `shiftL` 3) .|. (b2b b6 `shiftL` 2) .|. (b2b b7 `shiftL` 1) .|. b2b b8) : packInList rest
-- Special case: if we don't have enough bits to fill a byte, we'll pad with zeros
packInList ragged = [(foldl' (\ res b -> (res `shiftL` 1) + b2b b) 0 ragged) `shiftL` (8 - Prelude.length ragged)]


main :: IO()
main = do
  args <- getArgs
  case args of
    [infile, outfile] -> do
              Prelude.putStrLn $ "Infile: " ++ infile
              Prelude.putStrLn $ "Outfile: " ++ outfile
    _ -> Prelude.putStrLn "usage: readbits-hs <infile> <outfile>"