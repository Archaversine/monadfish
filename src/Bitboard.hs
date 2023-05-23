{-# LANGUAGE StrictData #-}

module Bitboard (Bitboard, 
                 getBit, 
                 popBit, 
                 popBits,
                 setBits,
                 popSquareBit,
                 popSquareBits,
                 setSquareBit,
                 setSquareBits,
                 printBitboard) where

import Data.Bits
import Data.Word (Word64)
import Data.List (intersperse, foldl')

import Squares

type Bitboard = Word64

-- ================= --
-- Bit Manipulations --
-- ================= --

getBit :: Int -> Bitboard -> Bitboard
getBit index board = board .&. bit index

setBits :: [Int] -> Bitboard -> Bitboard
setBits bits = foldl' (.) id (map (flip setBit) bits)

popBit :: Int -> Bitboard -> Bitboard 
popBit index board = board `xor` complement (bit index)

popBits :: [Int] -> Bitboard -> Bitboard
popBits bits = foldl' (.) id (map popBit bits)

popSquareBit :: BoardSquare -> Bitboard -> Bitboard 
popSquareBit square board = board .&. complement (bit $ fromEnum square)

popSquareBits :: [BoardSquare] -> Bitboard -> Bitboard 
popSquareBits squares = foldl' (.) id (map popSquareBit squares)

setSquareBit :: BoardSquare -> Bitboard -> Bitboard
setSquareBit square board = setBit board (fromEnum square)

setSquareBits :: [BoardSquare] -> Bitboard -> Bitboard
setSquareBits squares = foldl' (.) id (map setSquareBit squares)

-- =============== --
-- Debug Functions --
-- =============== --

printBitboard :: Bitboard -> IO ()
printBitboard board = do 
    mapM_ printRank [0..7] 
    putStrLn "   ---------------"
    putStrLn "   A B C D E F G H"
    putStrLn $ "\nBitboard: " <> show board <> "\n"
  where printRank rank = do 
            putStr   $ (toEnum $ 8 - rank + fromEnum '0' :: Char) : ": "
            putStrLn $ intersperse ' ' $ map (printSquare rank) [0..7]
        printSquare row col
          | testBit board (row * 8 + col) = '1'
          | otherwise                     = '0'
