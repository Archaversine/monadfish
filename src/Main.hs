module Main (main) where

import Board
import Attacks
import Bitboard

main :: IO ()
main = do    
    printBitboard $ maskPawnAttacks A4 White
    printBitboard $ maskPawnAttacks H4 White
    printBitboard $ maskPawnAttacks A4 Black
    printBitboard $ maskPawnAttacks H4 Black

