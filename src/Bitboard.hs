{-# LANGUAGE StrictData #-}

module Bitboard (Bitboard, 
                 notAFile,
                 notABFile,
                 notHFile,
                 notHGFile,
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

import Board

type Bitboard = Word64

-- ==================== --
-- Magic File Constants --
-- ==================== --

-- | Bitboard containing all 1s except for on the A file
-- 8: 0 1 1 1 1 1 1 1
-- 7: 0 1 1 1 1 1 1 1
-- 6: 0 1 1 1 1 1 1 1
-- 5: 0 1 1 1 1 1 1 1
-- 4: 0 1 1 1 1 1 1 1
-- 3: 0 1 1 1 1 1 1 1
-- 2: 0 1 1 1 1 1 1 1
-- 1: 0 1 1 1 1 1 1 1
--    ---------------
--    A B C D E F G H
notAFile :: Bitboard 
notAFile = 18374403900871474942

-- | Bitboard containing all 1s except for on the A & B files
-- 8: 0 0 1 1 1 1 1 1
-- 7: 0 0 1 1 1 1 1 1
-- 6: 0 0 1 1 1 1 1 1
-- 5: 0 0 1 1 1 1 1 1
-- 4: 0 0 1 1 1 1 1 1
-- 3: 0 0 1 1 1 1 1 1
-- 2: 0 0 1 1 1 1 1 1
-- 1: 0 0 1 1 1 1 1 1
--    ---------------
--    A B C D E F G H
notABFile :: Bitboard 
notABFile = 18229723555195321596

-- | Bitboard containing all 1s except for on the H file
-- 8: 1 1 1 1 1 1 1 0
-- 7: 1 1 1 1 1 1 1 0
-- 6: 1 1 1 1 1 1 1 0
-- 5: 1 1 1 1 1 1 1 0
-- 4: 1 1 1 1 1 1 1 0
-- 3: 1 1 1 1 1 1 1 0
-- 2: 1 1 1 1 1 1 1 0
-- 1: 1 1 1 1 1 1 1 0
--    ---------------
--    A B C D E F G H
notHFile :: Bitboard 
notHFile = 9187201950435737471

-- | Bitboard containing all 1s except for on the H & G files
-- 8: 1 1 1 1 1 1 0 0
-- 7: 1 1 1 1 1 1 0 0
-- 6: 1 1 1 1 1 1 0 0
-- 5: 1 1 1 1 1 1 0 0
-- 4: 1 1 1 1 1 1 0 0
-- 3: 1 1 1 1 1 1 0 0
-- 2: 1 1 1 1 1 1 0 0
-- 1: 1 1 1 1 1 1 0 0
--    ---------------
--    A B C D E F G H
notHGFile :: Bitboard 
notHGFile = 4557430888798830399

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
