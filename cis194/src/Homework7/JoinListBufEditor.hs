module Homework7.JoinListBufEditor where

import           Homework7.Buffer
import           Homework7.Editor
import           Homework7.JoinListBuffer


initialBuffer :: JoinListBuffer
initialBuffer = fromString $ unlines
                        [ "This buffer is for notes you don't want to save, and for"
                        , "evaluation of steam valve coefficients."
                        , "To load a different file, type the character L followed"
                        , "by the name of the file."
                        ]

main = runEditor editor initialBuffer
