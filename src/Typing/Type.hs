{-# LANGUAGE LambdaCase #-}

module Typing.Type  (
    Type(..),
    typeToList,
    foldTypes,
    rename,
) where

import Control.Monad.Trans.State (
    State,
    evalState,
    gets,
    modify,
    )
import Data.Binary

import Common.SrcPos
import Common.Var
import qualified Data.VarMap as M
import Text.Pretty


infixr 9 :->

data Type
    -- | A defined type (i.e. Int, Maybe a)
    = TypeCon Var [Type]
    | TypeVar Var
    -- | Application type (i.e. a -> b, a -> String)
    | Type :-> Type
    | TupleType [Type]
    | ArrayType Type
    deriving (Eq)


-- | Helpful for things like sum-type constructors
typeToList :: Type -> [Type]
typeToList (t1 :-> t2) = (t1:typeToList t2)
typeToList t = [t]

foldTypes :: [Type] -> Type -> Type
foldTypes [] typ = typ
foldTypes (t1:ts) t2 = t1 :-> foldTypes ts t2


instance HasSrcPos Type where
    getPos (TypeCon name _) = getPos name
    getPos (TypeVar name) = getPos name
    getPos (t1 :-> t2) = t1 <?> t2
    getPos (TupleType types) = foldr (<?>) UnknownPos types
    getPos (ArrayType typ) = getPos typ

instance Pretty Type where
    pretty (TypeCon name []) = pretty name
    pretty (TypeCon name types) = name|+" "+|" "`seps`types
    pretty (TypeVar name) = pretty name
    pretty (t1@(_ :-> _) :-> t2) = "("+|t1|+") -> "+|t2
    pretty (t1 :-> t2) = t1|+" -> "+|t2
    pretty (TupleType []) = "(,)"
    pretty (TupleType types) = "("+|", "`seps`types|+")"
    pretty (ArrayType typ) = "["+|typ|+"]"


rename :: Type -> Type
rename = flip evalState (M.empty, 0) . go
    where
        letters = (:[]) <$> ['a'..'z']
        go :: Type -> State (M.VarMap Var, Int) Type
        go (TypeCon name types) = TypeCon name <$> mapM go types
        go (TypeVar var) = gets (M.lookup var . fst) >>= \case
            Nothing -> do
                i <- gets snd
                let var' = Var (letters !! i) (getPos var)
                modify $ \(!m, !c) -> (M.insert var var' m, c + 1)
                return (TypeVar var')
            Just var' -> return (TypeVar var')
        go (t1 :-> t2) = (:->) <$> go t1 <*> go t2
        go (ArrayType typ) = ArrayType <$> go typ
        go (TupleType types) = TupleType <$> mapM go types


instance Binary Type where
    put (TypeCon name pars) = do
        putWord8 0
        put name
        putList pars
    -- the actual name of type-vars ultimately
    -- doesnt matter
    put (TypeVar _) = putWord8 1
    put (t1 :-> t2) = do
        putWord8 2
        put t1
        put t2
    put (TupleType types) = do
        putWord8 3
        putList types
    put (ArrayType typ) = do
        putWord8 4
        put typ

    get = do
        typ <- getWord8 >>= \case
            0 -> TypeCon <$> get <*> get
            -- arbitrary name ("a")
            1 -> return (TypeVar (prim ""))
            2 -> (:->) <$> get <*> get
            3 -> TupleType <$> get
            4 -> ArrayType <$> get
            n -> fail $ "Binary.get :: Type: "++
                "unknown flag ("+|n|+")"
        return $! rename typ
