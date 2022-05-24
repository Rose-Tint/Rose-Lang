module Common.Module (
    ModName(..),
    strToMod,
) where


infixr 9 :.
data ModName
    = String :. ModName
    | End String


strToMod :: String -> ModName
strToMod [] = End []
strToMod str =
    let (pre, post) = break (== '.') str
    in pre :. strToMod post
