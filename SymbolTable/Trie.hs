{-# LANGUAGE BangPatterns #-}

module SymbolTable.Trie (Trie,
    -- Construction
    empty, singleton, fromList,
    -- Insertion
    push, insert, insertWith,
    -- Query
    lookup, search, findWithDefault,
    assocsWithPrefix, keysWithPrefix, elemsWithPrefix,
    (??),
    -- Deletion/Updating
    delete, adjust, update,
    -- Combination
    union, -- difference, intersect,
    -- Other
    assocs, keys, elems,
    isEmpty, size, isMemberOf,
    -- Pretty
    prettyTrie, printTrie,
) where

import Prelude hiding (lookup)

import Data.Array hiding (
    assocs,
    elems
    )
import qualified Data.Array as A
import Data.Maybe (isNothing, fromMaybe)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup

-- import Debug.Trace (trace)

default (Int)



type Children a = Array Char (Trie a)


data Trie a
    = Empty
    | Link {
        -- array with indices that represent the
        -- character following `trieCommon`
        trieChildren :: {-# UNPACK #-} !(Children a),
        -- represents characters that are common
        -- amongst the children
        trieCommon :: String
    }
    | Node {
        trieChildren :: {-# UNPACK #-} !(Children a),
        trieValue :: !a
    }
    deriving (Show, Eq)



{- %%%%%%%%%% Construction %%%%%%%%%% -}

-- |Creates an empty trie
empty :: Trie a
{-# INLINE empty #-}
empty = Empty


-- |Creates a trie from a list of key-value pairs
fromList :: [(String, a)] -> Trie a
{-# INLINABLE fromList #-}
fromList [] = empty
fromList [(!str, !a)] = singleton str a
fromList ((!str, !a):rest) = insert str a $! fromList rest


-- |Creates a single-element trie
singleton :: String -> a -> Trie a
{-# INLINE singleton #-}
singleton [] a = node a
singleton [!c] !a = Link (oneChild c (node a)) []
singleton !str !a = let (cs, c) = (init str, last str)
    in Link (oneChild c (node a)) cs


-- |Inserts a value at the given key. If a value at
-- that key already exists, it will be replaced.
push :: String -> a -> Trie a -> Trie a
{-# INLINE push #-}
push = insertWith (flip const)


-- |Inserts a value at the given key. If a value at
-- that key already exists, nothing will happen.
insert :: String -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert = insertWith const

------------- trace ("~~~~~~~ HERE: " ++ ) $! 

-- |@`insertWith` f key val trie@ inserts @val@ at
-- @key@. If a value @old@ at that key already exists,
-- it is replaced with @f old val@.
insertWith :: (a -> a -> a) -> String -> a -> Trie a
           -> Trie a
insertWith _ [] !a (Link chn com) = Node (link chn com) a
insertWith f [] a1 (Node chn a2) =
    let !a = f `seq` f a2 a1 in Node chn a
insertWith _ str a Empty = singleton str a
insertWith _ [c1] a (Link chn com) =
    Link (linkAnd chn com [(c1, node a)]) []
insertWith f [c] a trie = updateChildAt c (insertWith f [] a) trie
insertWith f str a trie@(Link chn com) = case keyDiff str com of
    Equal -> let (cs, c) = (init str, last str) in
        a `seq` Link (oneChild c (Node chn a)) cs
    NoPrefix -> let (sc:scs) = str in
        Link (linkAnd chn com [(sc, singleton scs a)]) []
    RightEm _ (sc:|scs) ->
        updateChildAt sc (insertWith f scs a) trie
    -- i dont think this is correct, nor possible
    Diff (p:|ps) (sc:scs) com' ->
        Link (linkAnd chn com' [(sc, singleton scs a)]) (p:ps)
    Diff _ _ (c:cs) ->
        updateChildAt c (insertWith f cs a) trie
    LeftEm _ _ -> trie
    -- shouldn't be possible
    Diff _ [] [] -> error "keyDiff: left and right empty"
insertWith f (c:cs) a trie = updateChildAt c (insertWith f cs a) trie


{- %%%%%%%%%% Query %%%%%%%%%% -}


search :: String -> Trie a -> Trie a
{-# INLINABLE search #-}
search [] trie = trie
search _ Empty = Empty
search str@(c:cs) trie = case trie of
    Link chn com -> case keyDiff str com of
        -- only care about the left (`str`) after
        -- any prefix is removed
        RightEm _ (c':|cs') -> search cs' $! chn!c'
        Diff _ (c':cs') _ -> search cs' $! chn!c'
        _ -> Empty
    _ -> search cs $! getChildAt c trie


-- |Searches the trie for the given key. Returns
-- @`Just` val@ if found, and @`Nothing`@ otherwise
lookup :: String -> Trie a -> Maybe a
{-# INLINE lookup #-}
lookup str trie = case search str trie of
    Node _ a -> Just a
    _ -> Nothing


-- |Searches the trie for the given key, returning
-- the default value if not found.
findWithDefault :: a -> String -> Trie a -> a
{-# INLINE findWithDefault #-}
findWithDefault def str = fromMaybe def . lookup str


-- |Operator alias for @`flip` `lookup`@
infixl 9 ??
(??) :: Trie a -> String -> Maybe a
{-# INLINE (??) #-}
(??) = flip lookup


{- %%%%%%%%%% Deletion/Updating %%%%%%%%%% -}

-- |Deletes the value at the given key.
delete :: String -> Trie a -> Trie a
{-# INLINABLE delete #-}
delete [] trie = trie
delete [_] (Node chn _)
    | all isEmpty chn = Empty
    | otherwise = Link chn []
delete [_] trie = trie
delete (c:cs) trie = updateChildAt c (delete cs) trie


-- |Update the value at the given key with the given
-- function
adjust :: (a -> a) -> String -> Trie a -> Trie a
{-# INLINABLE adjust #-}
adjust _ _ Empty = Empty
adjust _ [] trie = trie
adjust f [_] trie@(Node _ a) = let !a' = f a in
    trie { trieValue = a' }
-- adjust [_] _ trie = trie
adjust f (c:cs) trie = updateChildAt c (adjust f cs) trie


-- |In the expression @`update` f key trie@, if
-- @f old@ is @`Nothing`@, it the value at @key@ is
-- deleted, but if it is @`Just` val@ then the value
-- at @key@ is @val@0
update :: (a -> Maybe a) -> String -> Trie a -> Trie a
{-# INLINABLE update #-}
update _ _ Empty = Empty
update _ [] trie = trie
update f [_] (Node chn a) = let a' = f a in case a' of
    Nothing -> Link chn [] -- effectively delete
    Just a'' -> Node chn a''
-- update [_] _ trie = trie
update f (c:cs) trie = updateChildAt c (update f cs) trie


{- %%%%%%%%%% Combining %%%%%%%%%% -}

-- |Creates a trie consisting of the all items in both tries.
-- Left preferential for items of shared keys.
union :: Trie a -> Trie a -> Trie a
union Empty trie = trie
union trie Empty = trie
union (Node chn a) trie = Node (zipChn union chn (trieChildren trie)) a
union trie (Node chn a) = Node (zipChn union (trieChildren trie) chn) a
union l1@(Link chn1 com1) l2@(Link chn2 com2)
    | all isEmpty chn1 = l2
    | all isEmpty chn2 = l1
    | otherwise = case keyDiff com1 com2 of
        Equal -> Link chn com1
        NoPrefix -> case (com1, com2) of
            ([], _) -> Link chn com2
            (_, []) -> Link chn com1
            ((c1:cs1), (c2:cs2)) -> Link (newChildren // [
                    (c1, Link chn1 cs1),
                    (c2, Link chn2 cs2)
                ]) []
        LeftEm pref (c:|cs) ->
            setChildAt c (Link chn2 cs) (Link chn1 pref)
        RightEm pref (c:|cs) ->
            setChildAt c (Link chn1 cs) (Link chn2 pref)
        -- i dont think this is correct, nor possible
        Diff (p:|ps) (c:cs) [] ->
            Link (oneChild c (Link chn1 cs)) (p:ps)
        -- i dont think this is correct, nor possible
        Diff (p:|ps) [] (c:cs) ->
            Link (oneChild c (Link chn2 cs)) (p:ps)
        Diff (p:|ps) (c1:cs1) (c2:cs2) -> Link
            (newChildren // [
                    (c1, Link chn1 cs1),
                    (c2, Link chn2 cs2)
                ]) (p:ps)
        -- i dont think this is correct, nor possible
        -- Diff (p:|ps) _ _ -> Link newChildren (p:ps)
        Diff _ [] [] -> error "keyDiff: left and right empty"
        where
            chn = zipChn union chn1 chn2


-- -- |Creates a trie consisting only of the common items.
-- -- Left preferential.
-- intersect :: Trie a -> Trie a -> Trie a
-- intersect Empty _ = Empty
-- intersect _ Empty = Empty
-- intersect (Node chn1 a) (Node chn2 _) =
--     let chn = zipChn intersect chn1 chn2 in Node chn a
-- intersect (Node chn1 _) (Link chn2 com) =
--     Link (zipChn intersect chn1 chn2) com
-- intersect l@(Link _ _) n@(Node _ _) = intersect n l
-- intersect (Link chn1 com1) (Link chn2 com2) = Link chn com
--     where
--         (com, uncom) = break (curry (==)) $! zip com1 com2
--         chn = if null uncom then
--                 zipChn intersect chn1 chn2
--             else
--                 zipChn (updateChildAt (head uncom)
--                     (intersect))
--                 zipChn intersect chn1 chn2
-- intersect tr1 tr2 = Link (zipChn intersect
--     (trieChildren tr1) (trieChildren tr2))


{- %%%%%%%%%% Other %%%%%%%%%% -}

-- |Returns a list of the key-value pairs
assocs :: Trie a -> [(String, a)]
{-# INLINABLE assocs #-}
assocs trie = case trie of
    Empty -> []
    Link chn com -> map' com chn
    Node chn a -> (([], a):map' [] chn)
    where
        map' done = concatMap (go done) . A.assocs
        go !done (c, trie') = case trie' of
            Empty -> []
            Link chn com -> map' (done ++ (c:com)) chn
            Node chn a -> let key = done ++ [c] in
                ((key, a):map' key chn)


-- |Returns a list of all keys in the trie
keys :: Trie a -> [String]
{-# INLINE keys #-}
keys Empty = []
keys (Link chn com) = concatMap (\(c, trie) ->
    fmap (\key -> com ++ (c:key)) (keys trie))
    (A.assocs chn)
keys (Node chn _) = concatMap (\(c, trie) ->
    fmap (c:) (keys trie))
    (A.assocs chn)


-- |Returns a list of all values in the trie
elems :: Trie a -> [a]
{-# INLINE elems #-}
elems Empty = []
elems (Link chn _) = foldr ((++) . elems) [] chn
elems (Node chn a) = foldr ((++) . elems) [a] chn


-- |Returns a list of key-value pairs, where the values
-- are all values whose key starts with the given string
assocsWithPrefix :: String -> Trie a -> [(String, a)]
{-# INLINABLE assocsWithPrefix #-}
assocsWithPrefix _ Empty = []
assocsWithPrefix pref (Link chn com) = concatMap (
    \(c,trie) -> assocsWithPrefix
        (pref ++ com ++ [c]) trie) (A.assocs chn)
assocsWithPrefix pref (Node chn a) = (pref, a) :
    concatMap (\(c,trie) -> assocsWithPrefix
        (pref ++ [c]) trie) (A.assocs chn)


-- |Returns a list of all keys that start with the given
-- string
keysWithPrefix :: String -> Trie a -> [String]
{-# INLINE keysWithPrefix #-}
keysWithPrefix pref =
    fmap (pref ++) . keys . search pref


-- |Returns a list of all values whose key starts with
-- the given string
elemsWithPrefix :: String -> Trie a -> [a]
{-# INLINE elemsWithPrefix #-}
elemsWithPrefix pref = elems . search pref 


-- |The size of the trie
size :: Trie a -> Int
{-# INLINE size #-}
size Empty = 0 :: Int
size (Link chn _) = foldl' (\b a ->
    b + size a) (0 :: Int) chn
size (Node chn _) = foldl' (\b a ->
    b + size a) (1 :: Int) chn


-- |Returns whether or not the trie is empty
isEmpty :: Trie a -> Bool
{-# INLINE isEmpty #-}
isEmpty Empty = True
isEmpty (Link chn _) = all isEmpty chn
isEmpty (Node _ _) = False


-- |Checks if the key exists in the trie
isMemberOf :: String -> Trie a -> Bool
{-# INLINE isMemberOf #-}
isMemberOf str = isNothing . lookup str
-- isMemberOf _ Empty = False
-- isMemberOf [] (Node _ _) = True
-- isMemberOf [] _ = False
-- isMemberOf str@(c:cs) (Link chn com) =
--     case keyDiff str com of
--         Equal -> False
--         NoPrefix -> False
-- isMemberOf str@(c:cs) (Node chn _) =
--     cs `isMemberOf` chn!c


{- %%%%%%%%%% Pretty %%%%%%%%%% -}

prettyTrie :: (Show a) => Trie a -> String
{-# NOINLINE prettyTrie #-}
prettyTrie = go (0 :: Int)
    where
        idt i = replicate (i * 4 :: Int) ' '
        go i Empty = idt i ++ "Empty"
        go i (Link chn com) = idt i ++ "Link: " ++
            show com ++ prettyChildren i chn
        go i (Node chn a) = idt i ++ "Node: " ++ show a
            ++ prettyChildren i chn
        prettyChildren i chn = concat
            [if isEmpty trie then "" else
                ('\n':idt (i + 1)) ++ show c ++ " : "
                 -- get rid of indentation
                ++ drop (succ i * 4) (go (i + 1) trie)
             | (c, trie) <- A.assocs chn]


printTrie :: (Show a) => Trie a -> IO ()
{-# NOINLINE printTrie #-}
printTrie = putStrLn . prettyTrie



{- %%%%%%%%%%%%%%%%%%%% INTERNALS %%%%%%%%%%%%%%%%%%%% -}


node :: a -> Trie a
{-# INLINE node #-}
node = Node newChildren


keyBounds :: (Char, Char)
{-# INLINE keyBounds #-}
keyBounds = (toEnum (0x20 :: Int), toEnum (0x7e :: Int))


mkChildren :: [(Char, Trie a)] -> Children a
{-# INLINE mkChildren #-}
mkChildren = array keyBounds


newChildren :: Children a
{-# INLINE newChildren #-}
newChildren = mkChildren [(k, Empty) | k <- range keyBounds]


oneChild :: Char -> Trie a -> Children a
{-# INLINE oneChild #-}
oneChild k trie = newChildren // [(k, trie)]


zipChn :: (Trie a -> Trie a -> Trie a)
            -> Children a -> Children a -> Children a
{-# INLINE zipChn #-}
zipChn !f !chn1 !chn2 = mkChildren [(k, f c1 c2) |
    k <- indices chn1,
    c1 <- A.elems chn1,
    c2 <- A.elems chn2]


link :: Children a -> String -> Children a
{-# INLINE link #-}
link !chn [] = chn
link !chn (c:cs) = oneChild c $ Link chn cs


linkAnd :: Children a -> String -> [(Char, Trie a)] -> Children a
{-# INLINE linkAnd #-}
linkAnd !chn [] [] = chn
linkAnd !chn [] !as = chn // as
linkAnd !chn (c:cs) !as = newChildren // ((c, Link chn cs):as)


getChildAt :: Char -> Trie a -> Trie a
{-# INLINE getChildAt #-}
getChildAt _ Empty = Empty
getChildAt !k !trie = trieChildren trie ! k


setChildAt :: Char -> Trie a -> Trie a -> Trie a
{-# INLINE setChildAt #-}
setChildAt _ _ Empty = Empty
setChildAt !k !child !trie = trie {
        trieChildren = trieChildren trie // [(k, child)]
    }


updateChildAt :: Char -> (Trie a -> Trie a) -> Trie a
              -> Trie a
{-# INLINE updateChildAt #-}
updateChildAt !k !f !trie = let !child = getChildAt k trie
    in setChildAt k (f child) trie


data KeyDiff
    = Equal
    | NoPrefix
    | LeftEm String (NonEmpty Char)
    | RightEm String (NonEmpty Char)
    | Diff (NonEmpty Char) String String
    deriving (Show, Eq)

keyDiff :: String -> String -> KeyDiff
{-# INLINE keyDiff #-}
keyDiff [] [] = Equal
keyDiff [] (c:cs) = LeftEm [] (c:|cs)
keyDiff (c:cs) [] = RightEm [] (c:|cs)
keyDiff (c1:cs1) (c2:cs2)
    | c1 == c2 = case keyDiff cs1 cs2 of
        Equal -> Equal
        NoPrefix -> Diff (c1:|[]) cs1 cs2
        LeftEm pref (c:|cs) -> Diff (c1:|pref) [] (c:cs)
        RightEm pref (c:|cs) -> Diff (c1:|pref) (c:cs) []
        Diff (p:|ps) cs1' cs2' -> Diff (c1:|p:ps) cs1' cs2'
    | otherwise = NoPrefix


instance Functor Trie where
    fmap _ Empty = Empty
    fmap f (Link chn com) = Link (fmap f <$> chn) com
    fmap f (Node chn a) = let !a' = f a in Node (fmap f <$> chn) a'
    _ <$ Empty = Empty
    a <$ Link chn com = Link (fmap (a <$) chn) com
    a <$ Node chn _ = Node (fmap (a <$) chn) a


instance Foldable Trie where
    foldr _ b Empty = b
    foldr f b (Link chn _) = foldr (\child b' -> case child of
        Empty -> b'
        Link chn' _ -> foldr (flip (foldr f)) b' chn'
        Node chn' a -> f a $! foldr (flip (foldr f)) b' chn'
        ) b chn
    foldr f b (Node chn a) = f a $! foldr (flip (foldr f)) b chn
    null = isEmpty


instance Semigroup (Trie a) where
    (<>) = union
    -- union is idempotent (i.e. trie `union` trie == trie).
    stimes = stimesIdempotentMonoid


instance Monoid (Trie a) where
    mempty = empty
