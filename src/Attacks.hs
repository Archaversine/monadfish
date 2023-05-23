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
        attacks = case color of
            White -> map (flip shiftR) [7, 9]
            Black -> map (flip shiftL) [7, 9]
        edges = notAFile .&. notHFile

    foldl' (\acc f -> acc .|. f piece) 0 attacks .&. edges
