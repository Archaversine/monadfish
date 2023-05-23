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

        -- List of functions that shifts the piece bit to 
        -- appropriate attack positions
        attacks = map (flip shifter) [7, 9]

        -- Mask out the attacks that go off the board
        edges = notAFile .&. notHFile

        -- Shift direction is dependent on piece color
        shifter = case color of 
            White -> shiftR 
            Black -> shiftL

    -- Apply the list of functions to the bitboard
    foldl' (\acc f -> acc .|. f piece) 0 attacks .&. edges
