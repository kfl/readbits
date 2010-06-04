-- Example of how to use the ByteString library to work with binary data.
-- Author: Ken Friis Larsen <ken@friislarsen.net>
-- Copyright: (c) 2010 Ken Friis Larsen

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC

import qualified Regexp as R

import Data.Word (Word8)
import Data.List (foldl')
import Data.Bits (shiftL, (.|.), (.&.), bit)
import System (getArgs)

data Bit = Z | O deriving (Show, Eq)

byteToBits :: Word8 -> [Bit]
byteToBits w = map (\m -> if w .&. m == 0 then Z else O) masks
    where masks = [bit i | i <- [1 .. 8]]

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

-- Example transformation, filter out all the zeros
transform :: [Bit] -> [Bit]
transform bits = filter (== O) bits


main :: IO()
main = do
  args <- getArgs
  case args of
    [infile, outfile] -> do
              bytestring <- BL.readFile infile
              let bits = [b | w <- BL.unpack bytestring, b <- byteToBits w]
              let transformed = transform bits
              let bytes = packInList transformed
              BL.writeFile outfile $ BL.pack bytes
    ["-copy", infile, outfile] -> do
              bytestring <- BL.readFile infile
              BL.writeFile outfile bytestring
    ["-regexp", infile, outfile] -> do
              bytestring <- BC.readFile infile
              print $ R.match' R.ex1 $ BC.unpack bytestring
    _ -> Prelude.putStrLn "usage: readbits-hs [-copy] <infile> <outfile>"