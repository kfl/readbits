-- Example of how to use the ByteString library to work with binary data.
-- Author: Ken Friis Larsen <ken@friislarsen.net>
-- Copyright: (c) 2010 Ken Friis Larsen

import qualified Data.ByteString.Lazy as BL

import Data.Word (Word8)
import Data.List (foldl')
import Data.Bits (shiftL, (.|.), (.&.), bit)
import System (getArgs)

data Bit = Z | O deriving (Show, Eq)

byteToBits :: Word8 -> [Bit]
byteToBits w = [if w .&. b == 0 then Z else O | i <- [1 .. 8], let b = bit i]

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
              bytestring <- BL.readFile infile
              let bits = [b | w <- BL.unpack bytestring, b <- byteToBits w]
              let ones = filter (== O) bits
              BL.writeFile outfile $ BL.pack $ packInList ones
    _ -> Prelude.putStrLn "usage: readbits-hs <infile> <outfile>"