module Text.RSTemplate.Parser.Utils where 

import qualified Data.ByteString.Char8 as C

findClosing open' close' text = findClosing' text 0 0
   where
    open  = C.pack open'
    close = C.pack close'
    findClosing' text i 0 = if i == 0 then findClosing' text i 1 else i
    findClosing' x  i n   | C.length x == 1 = i + 1
    findClosing' text i n = if open `C.isPrefixOf` text
                              then findClosing' (C.tail text) (i+1) (n+1)
                              else if close `C.isPrefixOf` text
                                     then findClosing' (C.tail text) (i+1) (n-1)
                                     else findClosing' (C.tail text) (i+1) n
