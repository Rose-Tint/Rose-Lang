module Utils.List (
    take',
    eTake,
    drop',
    slice,
    slice',
    split2',
) where


take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = (x:take' (n - 1) xs)

eTake :: Int -> [a] -> Either Int [a]
eTake n []
    | n <= 0 = Right []
    | otherwise = Left n
eTake n (x:xs) = case eTake (n - 1) xs of
    Left n' -> Left n'
    Right xs' -> Right (x:xs')

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
    | n <= 0 = (x:xs)
    | otherwise = drop' (n - 1) xs

slice :: Int -> Int -> [a] -> [a]
slice st end list
    | st == end = []
    | st > end = error "slice: start index > end index"
    | otherwise = case list of
        [] -> error "slice: range > length of list" -- st /= end
        (x:xs) ->
            if st > 0 then
                slice (st - 1) (end - 1) xs
            else if end > 0 then
                (x:slice 0 (end - 1) xs)
            else
                [x]

slice' :: Int -> Int -> [a] -> [a]
slice' _ _ [] = []
slice' st end (x:xs)
    | st >= end = []
    | st <= 0 = (x:slice' 0 (end - 1) xs)
    | end < 0 = []
    | otherwise = slice' (st - 1) (end - 1) xs

split2' :: Int -> Int -> [a] -> ([a], [a], [a])
split2' _ _ [] = ([], [], [])
split2' st end (x:xs)
    | st > end = ([], [], (x:xs))
    | st <= 0 =
        let (pre, mid, post) = split2' 0 (end - 1) xs
        in (pre, (x:mid), post)
    | end == 0 = ([], [x], xs)
    | end < 0 = ([], [], (x:xs))
    | otherwise =
        let (pre, mid, post) = split2' (st - 1) (end - 1) xs
        in ((x:pre), mid, post)
