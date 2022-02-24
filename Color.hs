module Color where


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


colored :: String -> Color -> String
colored str clr = show clr ++ (str ++ show Reset)


reset :: String -> String
reset str = show Reset ++ str


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
