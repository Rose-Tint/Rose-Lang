module Color (
    Color(..),
    color,
    colored,
    uncolor,
    reset,
    printf,
) where

import Text.Printf (
        PrintfType,
        PrintfArg(..),
        formatString
    )
import qualified Text.Printf (printf)


data Color
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Purple
    | Cyan
    | White
    | Reset


color :: String -> String
color [] = show Reset
color [ch] = [ch]
color ('\\':('$':rest)) = ('$':color rest)
color ('$':(ch:rest)) = clrStr ++ color rest
    where
        clrStr = case ch of
            'B' -> show Black
            'r' -> show Red
            'y' -> show Yellow
            'g' -> show Green
            'b' -> show Blue
            'p' -> show Purple
            'c' -> show Cyan
            'w' -> show White
            'R' -> show Reset
            _   -> ['$', ch]
color (c:cs) = (c:color cs)


colored :: String -> Color -> String
{-# INLINE colored #-}
colored str !clr = show clr ++ (str ++ show Reset)


reset :: String -> String
{-# INLINE reset #-}
reset str = show Reset ++ str


printf :: (PrintfType a) => String -> a
printf = Text.Printf.printf . color


uncolor :: String -> String
uncolor [] = []
uncolor ('\027':'[':'0':'m':str) = uncolor str
uncolor ('\027':'[':'3':_:'m':str) = uncolor str
uncolor (c:cs) = (c:uncolor cs)



instance Show Color where
    show Black  = "\027[30m"
    show Red    = "\027[31m"
    show Green  = "\027[32m"
    show Yellow = "\027[33m"
    show Blue   = "\027[34m"
    show Purple = "\027[35m"
    show Cyan   = "\027[36m"
    show White  = "\027[37m"
    show Reset  = "\027[0m"


instance PrintfArg Color where
    formatArg clr = formatString (show clr)
