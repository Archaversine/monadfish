{-# LANGUAGE StrictData #-}

module Attacks (generatePawnAttacks,
                maskPawnAttacks) where

import Board
import Bitboard 

import Data.Bits
import Data.List (foldl')

import qualified Data.Vector as V

type DoubleVec a = V.Vector (V.Vector a)

-- | Pawn Attacks table [side][square]
generatePawnAttacks :: DoubleVec Bitboard
generatePawnAttacks = undefined

maskPawnAttacks :: BoardSquare -> BoardColor -> Bitboard 
maskPawnAttacks square color = do 
    let piece = setSquareBit square 0
        attacks = map (flip shifter) [7, 9]
        edges = notAFile .&. notHFile
        shifter = case color of 
            White -> shiftR 
            Black -> shiftL

    foldl' (\acc f -> acc .|. f piece) 0 attacks .&. edges
