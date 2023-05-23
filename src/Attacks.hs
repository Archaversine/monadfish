{-# LANGUAGE StrictData #-}

module Attacks (generatePawnAttacks) where

import Bitboard (Bitboard)

import qualified Data.Vector as V

type DoubleVec a = V.Vector (V.Vector a)

-- | Pawn Attacks table [side][square]
generatePawnAttacks :: DoubleVec Bitboard
generatePawnAttacks = undefined

