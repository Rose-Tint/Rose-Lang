module Color (
    Color(..),
    color,
    colored,
    reset,
    printf,
) where

import Text.Printf (
    PrintfType,
    PrintfArg(..),
    formatString
    )
import qualified Text.Printf (printf)
import Text.Regex


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
colored str clr = show clr ++ (str ++ show Reset)


reset :: String -> String
{-# INLINE reset #-}
reset str = show Reset ++ str


printf :: (PrintfType a) => String -> a
printf = Text.Printf.printf . color


unColor :: String -> String
unColor str = subRegex reg "\0" str
    where
        reg = mkRegex "\x1B\[(0|3[0-7])m"



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
