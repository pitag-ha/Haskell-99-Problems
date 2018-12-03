module DecodeModified where

import EncodeModified (NE(..))

decodeModified :: [NE]  -> [Char]
decodeModified = concat . (map (\elt -> 
    case elt of Single a -> [a]
                Multiple n a -> take n $ repeat a))
