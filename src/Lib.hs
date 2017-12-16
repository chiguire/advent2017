module Lib
  ( execLib
  ) where

import Advent1
import Advent2
import Advent3
import Advent4
import Advent5
import Advent6
import Advent7
import Advent8
import Advent9
import Advent10
import Advent11
import Advent12
import Advent13
import Advent14


execLib :: String -> String
execLib x = case x of "advent1_1" -> show advent1_1
                      "advent1_2" -> show advent1_2
                      "advent2_1" -> show advent2_1
                      "advent2_2" -> show advent2_2
                      "advent3_1" -> show advent3_1
                      "advent3_2" -> show advent3_2
                      "advent4_1" -> show advent4_1
                      "advent4_2" -> show advent4_2
                      "advent5_1" -> show advent5_1
                      "advent5_2" -> show advent5_2
                      "advent6_1" -> show advent6_1
                      "advent6_2" -> show advent6_2
                      "advent7_1" -> show advent7_1
                      "advent7_2" -> show advent7_2
                      "advent8_1" -> show advent8_1
                      "advent8_2" -> show advent8_2
                      "advent9_1" -> show advent9_1
                      "advent9_2" -> show advent9_2
                      "advent10_1" -> show advent10_1
                      "advent10_2" -> show advent10_2
                      "advent11_1" -> show advent11_1
                      "advent11_2" -> show advent11_2
                      "advent12_1" -> show advent12_1
                      "advent12_2" -> show advent12_2
                      "advent13_1" -> show advent13_1
                      "advent13_2" -> show advent13_2
                      "advent14_1" -> show advent14_1
                      "advent14_2" -> show advent14_2
                      otherwise -> "Failed!"
