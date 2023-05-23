module Main (main) where

import Bitboard
import Board

main :: IO ()
main = do    
    let bitboard = setSquareBits [E4, C3, F2] 0

    printBitboard 0
    printBitboard bitboard
    printBitboard $ popSquareBit E4 bitboard


