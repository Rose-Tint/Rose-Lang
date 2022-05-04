{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Parser.Parser (rose) where

import Data.Array (listArray)

import Common.Item
import Common.Typing
import Common.Var
import Parser.Data
import Parser.Imports
import Parser.Lexer
import Pretty
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (Module)
happyIn4 :: (Module) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (Expr)
happyIn5 :: (Expr) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ([Expr])
happyIn6 :: ([Expr]) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ([Import])
happyIn7 :: ([Import]) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (Import)
happyIn8 :: (Import) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (Visibility)
happyIn9 :: (Visibility) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (Purity)
happyIn10 :: (Purity) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Mutability)
happyIn11 :: (Mutability) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Type)
happyIn12 :: (Type) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ([Type])
happyIn13 :: ([Type]) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ([Type])
happyIn14 :: ([Type]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ([Type])
happyIn15 :: ([Type]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ([Type])
happyIn16 :: ([Type]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ([Type])
happyIn17 :: ([Type]) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ([Type])
happyIn18 :: ([Type]) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Constraint)
happyIn19 :: (Constraint) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ([Var])
happyIn20 :: ([Var]) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ([Var])
happyIn21 :: ([Var]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 ([Var])
happyIn22 :: ([Var]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (Context)
happyIn23 :: (Context) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (Context)
happyIn24 :: (Context) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (TypeDecl)
happyIn25 :: (TypeDecl) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (Expr)
happyIn26 :: (Expr) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 ((Var, [Value]))
happyIn27 :: ((Var, [Value])) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ((Var, [Value]))
happyIn28 :: ((Var, [Value])) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ([Value])
happyIn29 :: ([Value]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ([Value])
happyIn30 :: ([Value]) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (Expr)
happyIn31 :: (Expr) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (Field)
happyIn32 :: (Field) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ([Field])
happyIn33 :: ([Field]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ([Field])
happyIn34 :: ([Field]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Ctor)
happyIn35 :: (Ctor) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ([Ctor])
happyIn36 :: ([Ctor]) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ([Ctor])
happyIn37 :: ([Ctor]) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (Expr)
happyIn38 :: (Expr) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Expr)
happyIn39 :: (Expr) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Context)
happyIn40 :: (Context) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (Expr)
happyIn41 :: (Expr) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 ([Expr])
happyIn42 :: ([Expr]) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (Expr)
happyIn43 :: (Expr) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 ([Expr])
happyIn44 :: ([Expr]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 ([Value])
happyIn45 :: ([Value]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ([Value])
happyIn46 :: ([Value]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (Value)
happyIn47 :: (Value) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ([Value])
happyIn48 :: ([Value]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ([Value])
happyIn49 :: ([Value]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (Value)
happyIn50 :: (Value) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (Value)
happyIn51 :: (Value) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (Value)
happyIn52 :: (Value) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (Value)
happyIn53 :: (Value) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (Value)
happyIn54 :: (Value) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (Value)
happyIn55 :: (Value) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 (Value)
happyIn56 :: (Value) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ([Value])
happyIn57 :: ([Value]) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (Value)
happyIn58 :: (Value) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ([Value])
happyIn59 :: ([Value]) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (Stmt)
happyIn60 :: (Stmt) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (Body)
happyIn61 :: (Body) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 ([Stmt])
happyIn62 :: ([Stmt]) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 ([Stmt])
happyIn63 :: ([Stmt]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (Body)
happyIn64 :: (Body) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (Stmt)
happyIn65 :: (Stmt) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 (Stmt)
happyIn66 :: (Stmt) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (Stmt)
happyIn67 :: (Stmt) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (Stmt)
happyIn68 :: (Stmt) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 (Body)
happyIn69 :: (Body) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 (Stmt)
happyIn70 :: (Stmt) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (Stmt)
happyIn71 :: (Stmt) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 ([MatchCase])
happyIn72 :: ([MatchCase]) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 ([MatchCase])
happyIn73 :: ([MatchCase]) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 (MatchCase)
happyIn74 :: (MatchCase) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 (Stmt)
happyIn75 :: (Stmt) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 (Stmt)
happyIn76 :: (Stmt) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 (Stmt)
happyIn77 :: (Stmt) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x02\x0e\x80\xa0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x84\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xf6\x20\x44\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xec\x41\x88\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x88\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x71\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\xe2\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\xc2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\xd8\x83\x50\xf1\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x42\xc4\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x3d\x08\x15\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\xc4\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x3d\x08\x15\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x44\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x40\x88\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x88\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x61\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x84\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x10\x22\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xf6\x20\x54\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x71\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\xe2\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x83\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\xd8\x83\x50\x71\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_rose","Module","TopLevelExpr","TopLevelExprs","Imports1","Import","Vis","Pur","Mut","Type","ArrowSepTypes1","ArrowSepTypes1_","CommaSepTypes2","CommaSepTypes2_","Types0","Types0_","Constraint","SmallIds1","SmallIds0","SmallIds0_","CtxSeq","CtxSeq_","TypeDecl","FuncDecl","FuncParamSeq","InfixParamSeq","PrefixParamSeq0","PrefixParamSeq0_","FuncDef","DataField","DataFields1","DataFields1_","CtorDef","PipeSepCtors","PipeSepCtors_","DataDef","TypeAlias","TraitCtx","TraitDecl","MethodDecls0","TraitImpl","MethodImpls0","Terms0","Terms0_","Term","ArrayTerms","TupleTerms","Lambda","CtorCall","Caller","FuncCall","Pattern","PatternItem","TuplePattern","TuplePtrns","CtorPattern","Patterns","Stmt","Body","Stmts0","Stmts0_","BodyAssignment","NullStmt","Expr","JumpStmt","Selection","StmtBody","IfElse","Match","Cases1","Cases1_","Case","Loop","NewVar","Reassignment","pure","impure","let","mut","intern","extern","import","using","return","if","else","match","loop","break","continue","impl","trait","data","\"=\"","\":\"","\";\"","\"|\"","\"->\"","\"=>\"","\",\"","\"(\"","\")\"","\"{\"","\"}\"","\"[\"","\"]\"","\"<\"","\">\"","literal","big_id","small_id","infix_id","prefix_id","\"_\"","%eof"]
        bit_start = st Prelude.* 117
        bit_end = (st Prelude.+ 1) Prelude.* 117
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..116]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x2f\x00\x2f\x00\x2f\x00\x00\x00\x01\x00\x33\x00\x34\x00\x3c\x00\x00\x00\x48\x01\x00\x00\x00\x00\x61\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x00\x00\x00\x00\x61\x00\x58\x00\x61\x00\x61\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x5d\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x76\x00\x00\x00\x00\x00\x90\x00\xc8\x00\xca\x00\xdd\x00\x79\x00\x00\x00\x00\x00\xa4\x01\x00\x00\xd4\x00\xdc\x00\xe6\x00\xa4\x01\x06\x01\xfd\x00\x00\x01\x00\x00\x00\x00\x00\x00\x2b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x01\x39\x02\x39\x02\x39\x02\x32\x02\x31\x01\x36\x01\x00\x00\x39\x02\x39\x02\x00\x00\x00\x00\x2f\x01\x00\x00\x00\x00\x39\x01\xdd\x00\xdd\x00\x00\x00\x00\x00\x00\x00\x2c\x01\x13\x00\x32\x01\x00\x00\x2e\x01\x33\x01\x79\x00\x42\x01\xb3\x00\x00\x00\x00\x00\x79\x00\x00\x00\x79\x00\x00\x00\x49\x01\x00\x00\x3a\x01\x46\x01\xdd\x00\x00\x00\x3b\x01\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x45\x01\x4c\x01\x4d\x01\x56\x01\x55\x01\x5a\x01\xdd\x00\x39\x02\x00\x00\x39\x02\x53\x01\xf4\xff\x00\x00\x43\x00\xc0\x00\x5e\x01\x5f\x01\x00\x00\x00\x00\x68\x01\x00\x00\xb4\x01\x31\x00\x68\x01\xef\xff\x57\x01\x00\x00\x00\x00\x00\x00\x39\x02\x00\x00\x86\x01\x00\x00\x00\x00\x00\x00\x59\x01\xdd\x00\x00\x00\x00\x00\x00\x00\x5c\x01\xbc\x00\x00\x00\x73\x01\x79\x00\x30\x00\xb4\x01\x00\x00\x00\x00\x00\x00\x39\x02\x00\x00\x39\x02\x00\x00\x39\x02\x00\x00\x60\x01\xf7\xff\x00\x00\xdd\x00\x00\x00\xdd\x00\x00\x00\xdd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x67\x01\x64\x01\x00\x00\x00\x00\x72\x01\x61\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x01\x6c\x01\x6c\x01\xb4\x01\x7c\x00\x79\x01\x79\x00\x00\x00\x86\x01\x84\x01\x39\x02\x00\x00\x77\x01\x00\x00\x15\x00\x39\x02\x00\x00\x00\x00\x00\x00\x00\x00\x88\x01\x00\x00\x00\x00\x43\x01\xe9\xff\x7e\x01\x00\x00\x00\x00\x8f\x01\x82\x01\xdd\x00\x00\x00\x00\x00\x00\x00\x86\x01\x16\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x00\x00\x92\x01\x9b\x01\x95\x01\x93\x01\x00\x00\xdd\x00\x94\x01\x00\x00\x00\x00\x00\x00\x99\x01\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xab\x00\xcd\x00\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x01\x00\x00\x00\x00\xb6\x01\x00\x00\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x01\x9d\x01\xb8\x01\xbe\x01\x54\x00\x00\x00\xc9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x01\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x8d\x01\x00\x00\x00\x00\x00\x00\xa1\x01\x00\x00\x5a\x00\xc5\x01\x9f\x01\x00\x00\x00\x00\x95\x00\xb7\x00\x00\x00\xc0\x01\x00\x00\xb6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x01\xba\x01\xc2\x01\xcf\x01\x24\x01\x00\x00\x00\x00\x00\x00\xae\x01\x6e\x01\x00\x00\xe9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x01\xda\x01\x16\x01\x1b\x01\x00\x00\x00\x00\x00\x00\xd3\x01\x1e\x01\x00\x00\x27\x01\xb2\x01\x00\x00\x00\x00\x00\x00\x00\x00\xb5\x01\x00\x00\xc6\x01\x00\x00\x00\x00\x00\x00\xd6\x01\x00\x00\xec\x01\x00\x00\x29\x01\x00\x00\xe8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\x01\xdd\x01\x00\x00\xe4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\x00\x00\xd7\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x01\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x01\x00\x00\x00\x00\xd1\xff\x00\x00\xde\x00\x00\x00\x00\x00\x00\x00\xf8\x01\x00\x00\xff\x01\x00\x00\x06\x02\x00\x00\x00\x00\x00\x00\x00\x00\xf4\x01\x00\x00\xfb\x01\x00\x00\xfc\x01\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x01\x00\x00\xe6\x01\x00\x00\x00\x00\x00\x00\x01\x02\xe1\x01\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x86\x00\x00\x00\xe0\xff\x00\x00\x53\x00\x00\x00\x13\x02\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x9c\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\x01\x00\x00\xc1\x00\xfd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xf7\xff\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\xfe\xff\xf5\xff\xf6\xff\xf0\xff\xfd\xff\x00\x00\xd0\xff\xfc\xff\xfb\xff\xfa\xff\xf9\xff\xf8\xff\x00\x00\xed\xff\xec\xff\xf0\xff\xbb\xff\xf0\xff\xf0\xff\x00\x00\x9b\xff\xcc\xff\x9c\xff\xf1\xff\xf2\xff\xcf\xff\xcd\xff\x00\x00\x98\xff\x97\xff\x00\x00\x99\xff\x92\xff\x00\x00\xee\xff\xef\xff\xbb\xff\x00\x00\xd6\xff\x00\x00\x00\x00\x87\xff\xca\xff\x00\x00\x8a\xff\x00\x00\x00\x00\x00\x00\x8b\xff\x00\x00\xd9\xff\x00\x00\xac\xff\xab\xff\xb1\xff\xaa\xff\x88\xff\x82\xff\x8e\xff\x8f\xff\x90\xff\x7e\xff\x7d\xff\x8d\xff\x85\xff\x84\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\xff\x00\x00\x00\x00\xaf\xff\xb1\xff\xd7\xff\xa1\xff\xce\xff\x00\x00\x00\x00\x00\x00\xdd\xff\xdd\xff\xd3\xff\x00\x00\x00\x00\x00\x00\xdd\xff\x00\x00\x00\x00\x93\xff\x00\x00\x00\x00\x9a\xff\xcb\xff\x00\x00\x96\xff\x00\x00\x91\xff\x00\x00\xd7\xff\x00\x00\x00\x00\xde\xff\xdb\xff\x00\x00\xd5\xff\x00\x00\xba\xff\xe8\xff\xe9\xff\x00\x00\xe2\xff\x00\x00\xe4\xff\x00\x00\xe1\xff\x00\x00\x00\x00\xa2\xff\xb2\xff\xa6\xff\x00\x00\xaa\xff\x00\x00\x00\x00\xac\xff\xaa\xff\x80\xff\x81\xff\x00\x00\x71\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\x83\xff\x9d\xff\x00\x00\xd8\xff\x00\x00\x89\xff\x8c\xff\xd1\xff\xd6\xff\x00\x00\x7c\xff\x7b\xff\xa3\xff\x9e\xff\x00\x00\x7f\xff\x79\xff\x00\x00\xaa\xff\x00\x00\x72\xff\x9f\xff\xa0\xff\x00\x00\xad\xff\x00\x00\xa9\xff\xa7\xff\xae\xff\xb0\xff\x00\x00\xbc\xff\x00\x00\xe6\xff\x00\x00\xe5\xff\x00\x00\xe7\xff\xd4\xff\xda\xff\xdc\xff\xb4\xff\x00\x00\x00\x00\x94\xff\x95\xff\xbe\xff\xf0\xff\xb8\xff\x00\x00\xdf\xff\xe3\xff\xe0\xff\x6e\xff\xa8\xff\xa4\xff\xa5\xff\x00\x00\x00\x00\x00\x00\x77\xff\x75\xff\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\xd2\xff\x00\x00\x00\x00\x7a\xff\x76\xff\x78\xff\x74\xff\x00\x00\xb3\xff\xb5\xff\x00\x00\xc3\xff\x00\x00\xbf\xff\xbd\xff\xc1\xff\x00\x00\x00\x00\xb7\xff\xb6\xff\xb9\xff\x00\x00\x00\x00\x6f\xff\x70\xff\x73\xff\x00\x00\xc5\xff\x00\x00\xc8\xff\x00\x00\x00\x00\xc0\xff\x00\x00\xc6\xff\xc2\xff\xc4\xff\xc7\xff\x00\x00\xc9\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x11\x00\x12\x00\x32\x00\x15\x00\x1c\x00\x05\x00\x06\x00\x06\x00\x20\x00\x17\x00\x18\x00\x15\x00\x19\x00\x1b\x00\x08\x00\x09\x00\x0a\x00\x32\x00\x1f\x00\x25\x00\x44\x00\x45\x00\x46\x00\x16\x00\x17\x00\x18\x00\x2b\x00\x25\x00\x1b\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x23\x00\x32\x00\x46\x00\x14\x00\x38\x00\x39\x00\x15\x00\x15\x00\x19\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x32\x00\x1a\x00\x07\x00\x47\x00\x48\x00\x49\x00\x25\x00\x25\x00\x2b\x00\x22\x00\x23\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x15\x00\x1f\x00\x20\x00\x21\x00\x38\x00\x39\x00\x1b\x00\x13\x00\x1c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x1c\x00\x25\x00\x23\x00\x47\x00\x48\x00\x49\x00\x28\x00\x19\x00\x2b\x00\x1b\x00\x23\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x05\x00\x06\x00\x25\x00\x0f\x00\x38\x00\x39\x00\x25\x00\x13\x00\x14\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x1d\x00\x1e\x00\x20\x00\x47\x00\x48\x00\x49\x00\x1f\x00\x24\x00\x2b\x00\x26\x00\x27\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x33\x00\x34\x00\x0f\x00\x36\x00\x38\x00\x39\x00\x13\x00\x14\x00\x13\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x1e\x00\x1c\x00\x23\x00\x47\x00\x48\x00\x49\x00\x24\x00\x39\x00\x2b\x00\x27\x00\x3c\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x38\x00\x39\x00\x03\x00\x04\x00\x20\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x1c\x00\x1d\x00\x1e\x00\x47\x00\x48\x00\x49\x00\x32\x00\x39\x00\x2b\x00\x35\x00\x3c\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x08\x00\x09\x00\x0a\x00\x19\x00\x38\x00\x1b\x00\x13\x00\x03\x00\x04\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x1a\x00\x42\x00\x43\x00\x19\x00\x1e\x00\x1b\x00\x47\x00\x48\x00\x49\x00\x23\x00\x24\x00\x2b\x00\x19\x00\x1a\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x02\x00\x23\x00\x04\x00\x23\x00\x38\x00\x11\x00\x12\x00\x3a\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x1a\x00\x42\x00\x43\x00\x26\x00\x1e\x00\x20\x00\x47\x00\x48\x00\x49\x00\x23\x00\x24\x00\x2b\x00\x1d\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2b\x00\x29\x00\x2a\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x29\x00\x2a\x00\x3d\x00\x3e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x3d\x00\x3e\x00\x01\x00\x18\x00\x48\x00\x49\x00\x24\x00\x06\x00\x0d\x00\x0e\x00\x25\x00\x48\x00\x49\x00\x0d\x00\x0e\x00\x2b\x00\x0d\x00\x0e\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x04\x00\x16\x00\x17\x00\x18\x00\x11\x00\x12\x00\x1b\x00\x11\x00\x12\x00\x11\x00\x12\x00\x3d\x00\x3e\x00\x22\x00\x23\x00\x15\x00\x25\x00\x13\x00\x27\x00\x01\x00\x02\x00\x15\x00\x48\x00\x49\x00\x01\x00\x02\x00\x15\x00\x13\x00\x21\x00\x32\x00\x2b\x00\x08\x00\x23\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x24\x00\x24\x00\x10\x00\x11\x00\x12\x00\x19\x00\x13\x00\x39\x00\x24\x00\x24\x00\x1d\x00\x1e\x00\x1c\x00\x23\x00\x1f\x00\x19\x00\x1e\x00\x24\x00\x1b\x00\x26\x00\x27\x00\x03\x00\x24\x00\x17\x00\x26\x00\x27\x00\x1b\x00\x09\x00\x0a\x00\x19\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x25\x00\x1b\x00\x1b\x00\x24\x00\x23\x00\x15\x00\x0b\x00\x11\x00\x12\x00\x25\x00\x1a\x00\x1c\x00\x1c\x00\x25\x00\x1e\x00\x23\x00\x16\x00\x03\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x09\x00\x0a\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x1d\x00\x13\x00\x21\x00\x2b\x00\x2c\x00\x15\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\x23\x00\x1c\x00\x1b\x00\x1e\x00\x16\x00\x24\x00\x03\x00\x22\x00\x23\x00\x24\x00\x21\x00\x26\x00\x09\x00\x0a\x00\x1d\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x19\x00\x20\x00\x23\x00\x03\x00\x24\x00\x15\x00\x21\x00\x05\x00\x05\x00\x05\x00\x1a\x00\x11\x00\x12\x00\x24\x00\x1e\x00\x05\x00\x37\x00\x24\x00\x22\x00\x23\x00\x24\x00\x15\x00\x26\x00\x11\x00\x12\x00\x08\x00\x1a\x00\x07\x00\x32\x00\x32\x00\x1e\x00\x11\x00\x12\x00\x15\x00\x22\x00\x23\x00\x24\x00\x2b\x00\x26\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x08\x00\x10\x00\x32\x00\x2b\x00\x10\x00\x32\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x28\x00\x2b\x00\x11\x00\x12\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x08\x00\x11\x00\x12\x00\x0f\x00\x32\x00\x08\x00\x2b\x00\x08\x00\x08\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x11\x00\x12\x00\x08\x00\x08\x00\x1f\x00\x05\x00\x26\x00\x2b\x00\x11\x00\x12\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2b\x00\x11\x00\x12\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1f\x00\x11\x00\x12\x00\x1c\x00\xff\xff\x2b\x00\xff\xff\xff\xff\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\x2b\x00\x11\x00\x12\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2b\x00\x11\x00\x12\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2b\x00\xff\xff\xff\xff\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\xff\xff\x2e\x00\x2f\x00\x30\x00\x31\x00\x2b\x00\xff\xff\xff\xff\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\xff\xff\x1c\x00\xff\xff\x1e\x00\xff\xff\xff\xff\x1a\x00\x22\x00\x23\x00\x24\x00\x1e\x00\x26\x00\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x3a\x00\x3b\x00\xd0\x00\xa6\x00\xea\x00\x07\x00\x08\x00\x0c\x00\xeb\x00\x0e\x00\x0f\x00\xcc\x00\xb2\x00\xe1\x00\xd7\x00\xd8\x00\x7e\x00\xd0\x00\xb3\x00\x99\x00\xd1\x00\xd2\x00\xd3\x00\xeb\x00\x0e\x00\x0f\x00\x3c\x00\x99\x00\xec\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\x09\x00\x15\x00\xdd\x00\x77\x00\xa0\x00\xa1\x00\xf1\x00\xf2\x00\x78\x00\x42\x00\x43\x00\x44\x00\x45\x00\xaa\x00\x46\x00\x47\x00\x15\x00\x28\x00\x05\x00\x48\x00\x49\x00\x4a\x00\x99\x00\x99\x00\x3c\x00\x29\x00\x2a\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\x97\x00\xe6\x00\xe7\x00\xe8\x00\xa0\x00\xa1\x00\xac\x00\x35\x00\xa8\x00\x42\x00\x43\x00\x44\x00\x45\x00\xa6\x00\x46\x00\x47\x00\x36\x00\x99\x00\x22\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xb0\x00\x3c\x00\xb1\x00\x21\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\x2c\x00\x2d\x00\x99\x00\x5f\x00\xa0\x00\xa1\x00\x32\x00\x60\x00\x61\x00\x42\x00\x43\x00\x44\x00\x45\x00\xa2\x00\x46\x00\x47\x00\xe3\x00\x1d\x00\x30\x00\x48\x00\x49\x00\x4a\x00\x6a\x00\x1e\x00\x3c\x00\x1f\x00\x20\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\x24\x00\x25\x00\x5f\x00\x26\x00\xa0\x00\xa1\x00\x9f\x00\x61\x00\x35\x00\x42\x00\x43\x00\x44\x00\x45\x00\xdc\x00\x46\x00\x47\x00\x1d\x00\x36\x00\x66\x00\x48\x00\x49\x00\x4a\x00\x1e\x00\x32\x00\x3c\x00\x20\x00\x33\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\xd7\x00\xf3\x00\x7e\x00\x05\x00\xa0\x00\xa1\x00\x02\x00\x03\x00\x30\x00\x42\x00\x43\x00\x44\x00\x45\x00\xf2\x00\x46\x00\x47\x00\xf4\x00\xf5\x00\xf6\x00\x48\x00\x49\x00\x4a\x00\x67\x00\x32\x00\x3c\x00\x68\x00\xdf\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\xd7\x00\xff\x00\x7e\x00\x6c\x00\x41\x00\x6d\x00\xd7\x00\x02\x00\x03\x00\x42\x00\x43\x00\x44\x00\x45\x00\x5c\x00\x46\x00\x47\x00\xae\x00\x5d\x00\xaf\x00\x48\x00\x49\x00\x4a\x00\x5e\x00\x5f\x00\x3c\x00\x22\x00\x23\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\x09\x00\x64\x00\x0a\x00\x63\x00\x9b\x00\x3a\x00\x3b\x00\x38\x00\x39\x00\x42\x00\x43\x00\x44\x00\x45\x00\x5c\x00\x46\x00\x47\x00\x38\x00\x5d\x00\x9f\x00\x48\x00\x49\x00\x4a\x00\x5e\x00\x5f\x00\x88\x00\x9d\x00\x89\x00\x8a\x00\x3e\x00\x3f\x00\xa8\x00\x3c\x00\x97\x00\x84\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x3a\x00\x3b\x00\x83\x00\x84\x00\x42\x00\xa9\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x42\x00\xcf\x00\x0b\x00\x9b\x00\x49\x00\x4a\x00\x9a\x00\x0c\x00\x7a\x00\x73\x00\x99\x00\x49\x00\x4a\x00\x79\x00\x73\x00\x3c\x00\x72\x00\x73\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x96\x00\x0d\x00\x0e\x00\x0f\x00\x3a\x00\x3b\x00\x10\x00\x6f\x00\x3b\x00\xbd\x00\x3b\x00\x42\x00\xe0\x00\x11\x00\x12\x00\x97\x00\x13\x00\x83\x00\x14\x00\x17\x00\x18\x00\x8e\x00\x49\x00\x4a\x00\x17\x00\x18\x00\x8d\x00\x82\x00\x79\x00\x15\x00\x8e\x00\x19\x00\x72\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\x76\x00\x71\x00\x1a\x00\x1b\x00\x1c\x00\x6e\x00\xc2\x00\x8f\x00\x76\x00\x71\x00\xee\x00\x1d\x00\xc0\x00\x63\x00\xbc\x00\xbb\x00\x1d\x00\x1e\x00\xba\x00\x1f\x00\x20\x00\x4c\x00\x1e\x00\xb9\x00\x1f\x00\x20\x00\xb8\x00\x4d\x00\x4e\x00\xb7\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x99\x00\xad\x00\xac\x00\xa5\x00\x63\x00\x53\x00\xd5\x00\x3a\x00\x3b\x00\x00\x00\x54\x00\xc7\x00\x36\x00\x99\x00\x55\x00\xc6\x00\xe6\x00\x4c\x00\x56\x00\x57\x00\x58\x00\x99\x00\x59\x00\x4d\x00\x4e\x00\x99\x00\x4f\x00\x50\x00\x51\x00\x52\x00\xdf\x00\xdc\x00\xda\x00\x85\x00\x86\x00\x53\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\x54\x00\xc6\x00\x36\x00\xef\x00\x55\x00\xf9\x00\xf8\x00\x4c\x00\x56\x00\x57\x00\x58\x00\xfe\x00\x59\x00\x4d\x00\x4e\x00\xfd\x00\x4f\x00\x50\x00\x51\x00\x52\x00\xfc\x00\xfb\x00\xc6\x00\x4c\x00\xf8\x00\x53\x00\x01\x01\x36\x00\x30\x00\x2d\x00\x54\x00\x3a\x00\x3b\x00\x2e\x00\x55\x00\x2a\x00\x66\x00\x64\x00\x56\x00\x57\x00\x58\x00\x53\x00\x59\x00\x3a\x00\x3b\x00\x5a\x00\x54\x00\x94\x00\x6a\x00\x59\x00\x55\x00\x3a\x00\x3b\x00\x9d\x00\x56\x00\x57\x00\x58\x00\x88\x00\x59\x00\x89\x00\x8a\x00\x3e\x00\x3f\x00\x8b\x00\x3a\x00\x3b\x00\x7b\x00\x74\x00\x6e\x00\x93\x00\xc0\x00\xc3\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xc7\x00\x92\x00\x3a\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xbe\x00\x3a\x00\x3b\x00\xbc\x00\xc2\x00\xb5\x00\x91\x00\xd5\x00\xca\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\x3a\x00\x3b\x00\xc9\x00\xc8\x00\xc4\x00\xe4\x00\xe3\x00\xb4\x00\x3a\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xb3\x00\x3a\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xf9\x00\x3a\x00\x3b\x00\xfe\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\x00\x00\xce\x00\x3a\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xcd\x00\x3a\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xcc\x00\x00\x00\x00\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x00\x00\x00\x00\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\xef\x00\x00\x00\x00\x00\x3d\x00\x3e\x00\x3f\x00\x87\x00\x91\x00\x00\x00\x36\x00\x00\x00\x55\x00\x00\x00\x00\x00\x54\x00\x56\x00\x57\x00\x71\x00\x55\x00\x59\x00\x00\x00\x00\x00\x56\x00\x57\x00\x71\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 145) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145)
	]

happy_n_terms = 41 :: Prelude.Int
happy_n_nonterms = 74 :: Prelude.Int

happyReduce_1 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
	happyIn4
		 (Module happy_var_1 happy_var_2
	)}}

happyReduce_2 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_3 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_5 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_6 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_6 = happySpecReduce_1  1# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_7 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_7 = happySpecReduce_1  1# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_8 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_8 = happySpecReduce_0  2# happyReduction_8
happyReduction_8  =  happyIn6
		 ([]
	)

happyReduce_9 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_9 = happySpecReduce_2  2# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn6
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_10 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_10 = happySpecReduce_2  3# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	happyIn7
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_11 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_11 = happySpecReduce_1  3# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn7
		 ([happy_var_1]
	)}

happyReduce_12 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_12 = happySpecReduce_2  4# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TBig happy_var_2) -> 
	happyIn8
		 (Import happy_var_2 Intern
	)}

happyReduce_13 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_13 = happySpecReduce_3  4# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_3 of { (TBig happy_var_3) -> 
	happyIn8
		 (Import happy_var_3 Intern
	)}

happyReduce_14 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_14 = happySpecReduce_3  4# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_3 of { (TBig happy_var_3) -> 
	happyIn8
		 (Import happy_var_3 Extern
	)}

happyReduce_15 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_15 = happySpecReduce_0  5# happyReduction_15
happyReduction_15  =  happyIn9
		 (Extern
	)

happyReduce_16 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_16 = happySpecReduce_1  5# happyReduction_16
happyReduction_16 happy_x_1
	 =  happyIn9
		 (Extern
	)

happyReduce_17 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_17 = happySpecReduce_1  5# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn9
		 (Intern
	)

happyReduce_18 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_18 = happySpecReduce_1  6# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn10
		 (Pure
	)

happyReduce_19 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_19 = happySpecReduce_1  6# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn10
		 (Impure
	)

happyReduce_20 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_20 = happySpecReduce_0  7# happyReduction_20
happyReduction_20  =  happyIn11
		 (Pure
	)

happyReduce_21 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn11
		 (Impure
	)

happyReduce_22 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_22 = happySpecReduce_2  8# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn12
		 (Type happy_var_1 happy_var_2
	)}}

happyReduce_23 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_23 = happySpecReduce_2  8# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TSmall happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn12
		 (Param happy_var_1 happy_var_2
	)}}

happyReduce_24 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_24 = happySpecReduce_3  8# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn12
		 (Type (prim "[]") [happy_var_2]
	)}

happyReduce_25 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_25 = happySpecReduce_3  8# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn12
		 (Type (prim ",") happy_var_2
	)}

happyReduce_26 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_26 = happySpecReduce_3  8# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
	happyIn12
		 (Applied happy_var_2
	)}

happyReduce_27 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_27 = happySpecReduce_1  9# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn13
		 (reverse happy_var_1
	)}

happyReduce_28 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_28 = happySpecReduce_3  10# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn14
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_29 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_29 = happySpecReduce_1  10# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_30 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_30 = happySpecReduce_1  11# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn15
		 (reverse happy_var_1
	)}

happyReduce_31 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_31 = happySpecReduce_3  12# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn16
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_32 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_32 = happySpecReduce_3  12# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn16
		 ([happy_var_3, happy_var_1]
	)}}

happyReduce_33 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_33 = happySpecReduce_1  13# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn17
		 (reverse happy_var_1
	)}

happyReduce_34 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_34 = happySpecReduce_0  14# happyReduction_34
happyReduction_34  =  happyIn18
		 ([]
	)

happyReduce_35 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_35 = happySpecReduce_2  14# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn18
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_36 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_36 = happySpecReduce_2  15# happyReduction_36
happyReduction_36 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn19
		 (Constraint happy_var_1 happy_var_2
	)}}

happyReduce_37 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_37 = happySpecReduce_2  16# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TSmall happy_var_1) -> 
	case happyOut21 happy_x_2 of { (HappyWrap21 happy_var_2) -> 
	happyIn20
		 ((happy_var_1:happy_var_2)
	)}}

happyReduce_38 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_38 = happySpecReduce_1  17# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn21
		 (reverse happy_var_1
	)}

happyReduce_39 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_39 = happySpecReduce_2  18# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	case happyOutTok happy_x_2 of { (TSmall happy_var_2) -> 
	happyIn22
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_40 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_40 = happySpecReduce_1  18# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TSmall happy_var_1) -> 
	happyIn22
		 ([happy_var_1]
	)}

happyReduce_41 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_41 = happySpecReduce_0  19# happyReduction_41
happyReduction_41  =  happyIn23
		 ([]
	)

happyReduce_42 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_42 = happySpecReduce_2  19# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn23
		 (reverse happy_var_1
	)}

happyReduce_43 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_43 = happySpecReduce_3  20# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn24
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_44 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_44 = happySpecReduce_1  20# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn24
		 ([happy_var_1]
	)}

happyReduce_45 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_45 = happyReduce 4# 21# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
	happyIn25
		 (TypeDecl happy_var_2 (Applied happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_46 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_46 = happyReduce 4# 22# happyReduction_46
happyReduction_46 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TPrefix happy_var_3) -> 
	case happyOut25 happy_x_4 of { (HappyWrap25 happy_var_4) -> 
	happyIn26
		 (FuncDecl happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_47 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_47 = happySpecReduce_1  23# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_48 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_48 = happySpecReduce_2  23# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TPrefix happy_var_1) -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn27
		 ((happy_var_1, happy_var_2)
	)}}

happyReduce_49 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_49 = happySpecReduce_3  24# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOutTok happy_x_2 of { (TInfix happy_var_2) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn28
		 ((happy_var_2, [happy_var_1, happy_var_3])
	)}}}

happyReduce_50 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_50 = happySpecReduce_1  25# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn29
		 (reverse happy_var_1
	)}

happyReduce_51 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_51 = happySpecReduce_0  26# happyReduction_51
happyReduction_51  =  happyIn30
		 ([]
	)

happyReduce_52 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_52 = happySpecReduce_2  26# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	case happyOut54 happy_x_2 of { (HappyWrap54 happy_var_2) -> 
	happyIn30
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_53 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_53 = happySpecReduce_2  27# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	happyIn31
		 (let (name, pars) = happy_var_1 in FuncDef name pars happy_var_2
	)}}

happyReduce_54 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_54 = happyReduce 4# 28# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TSmall happy_var_1) -> 
	case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
	happyIn32
		 (Field happy_var_1 (normalize (Applied happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_55 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_55 = happySpecReduce_1  29# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn33
		 (reverse happy_var_1
	)}

happyReduce_56 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_56 = happySpecReduce_3  30# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut32 happy_x_3 of { (HappyWrap32 happy_var_3) -> 
	happyIn34
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_57 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_57 = happySpecReduce_2  30# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_58 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_58 = happySpecReduce_1  30# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn34
		 ([happy_var_1]
	)}

happyReduce_59 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_59 = happyReduce 5# 31# happyReduction_59
happyReduction_59 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut13 happy_x_4 of { (HappyWrap13 happy_var_4) -> 
	happyIn35
		 (SumType happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_60 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_60 = happySpecReduce_2  31# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn35
		 (SumType happy_var_1 happy_var_2 []
	)}}

happyReduce_61 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_61 = happyReduce 5# 31# happyReduction_61
happyReduction_61 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut33 happy_x_4 of { (HappyWrap33 happy_var_4) -> 
	happyIn35
		 (Record happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_62 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_62 = happySpecReduce_1  32# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn36
		 (reverse happy_var_1
	)}

happyReduce_63 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_63 = happySpecReduce_3  33# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut35 happy_x_3 of { (HappyWrap35 happy_var_3) -> 
	happyIn37
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_64 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_64 = happySpecReduce_1  33# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn37
		 ([happy_var_1]
	)}

happyReduce_65 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_65 = happyReduce 6# 34# happyReduction_65
happyReduction_65 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TBig happy_var_3) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	case happyOut35 happy_x_6 of { (HappyWrap35 happy_var_6) -> 
	happyIn38
		 (DataDef happy_var_2 happy_var_3 happy_var_4 [happy_var_6]
	) `HappyStk` happyRest}}}}

happyReduce_66 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_66 = happyReduce 8# 34# happyReduction_66
happyReduction_66 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TBig happy_var_3) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	case happyOut35 happy_x_6 of { (HappyWrap35 happy_var_6) -> 
	case happyOut36 happy_x_8 of { (HappyWrap36 happy_var_8) -> 
	happyIn38
		 (DataDef happy_var_2 happy_var_3 happy_var_4 (happy_var_6:happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_67 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_67 = happyReduce 5# 35# happyReduction_67
happyReduction_67 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut12 happy_x_5 of { (HappyWrap12 happy_var_5) -> 
	happyIn39
		 (TypeAlias happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_68 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_68 = happySpecReduce_0  36# happyReduction_68
happyReduction_68  =  happyIn40
		 ([]
	)

happyReduce_69 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_69 = happySpecReduce_3  36# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	happyIn40
		 (happy_var_2
	)}

happyReduce_70 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_70 = happyReduce 8# 37# happyReduction_70
happyReduction_70 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut40 happy_x_3 of { (HappyWrap40 happy_var_3) -> 
	case happyOutTok happy_x_4 of { (TBig happy_var_4) -> 
	case happyOut20 happy_x_5 of { (HappyWrap20 happy_var_5) -> 
	case happyOut42 happy_x_7 of { (HappyWrap42 happy_var_7) -> 
	happyIn41
		 (TraitDecl happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}}

happyReduce_71 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_71 = happySpecReduce_0  38# happyReduction_71
happyReduction_71  =  happyIn42
		 ([]
	)

happyReduce_72 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_72 = happySpecReduce_2  38# happyReduction_72
happyReduction_72 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut26 happy_x_2 of { (HappyWrap26 happy_var_2) -> 
	happyIn42
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_73 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_73 = happySpecReduce_2  38# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	happyIn42
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_74 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_74 = happyReduce 7# 39# happyReduction_74
happyReduction_74 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_2 of { (HappyWrap40 happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TBig happy_var_3) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	case happyOut44 happy_x_6 of { (HappyWrap44 happy_var_6) -> 
	happyIn43
		 (TraitImpl happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_75 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_75 = happySpecReduce_0  40# happyReduction_75
happyReduction_75  =  happyIn44
		 ([]
	)

happyReduce_76 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_76 = happySpecReduce_2  40# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	happyIn44
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_77 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_77 = happySpecReduce_1  41# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn45
		 (reverse happy_var_1
	)}

happyReduce_78 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_78 = happySpecReduce_0  42# happyReduction_78
happyReduction_78  =  happyIn46
		 ([]
	)

happyReduce_79 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_79 = happySpecReduce_2  42# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn46
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_80 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_80 = happySpecReduce_1  43# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TValue happy_var_1) -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_81 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_81 = happySpecReduce_3  43# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn47
		 (mkArray happy_var_2
	)}

happyReduce_82 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_82 = happySpecReduce_3  43# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	happyIn47
		 (mkTuple happy_var_2
	)}

happyReduce_83 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_83 = happySpecReduce_1  43# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_84 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_84 = happySpecReduce_1  43# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_85 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_85 = happySpecReduce_1  43# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_86 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_86 = happySpecReduce_3  43# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn47
		 (happy_var_2
	)}

happyReduce_87 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_87 = happySpecReduce_3  44# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	happyIn48
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_88 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_88 = happySpecReduce_2  44# happyReduction_88
happyReduction_88 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_89 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_89 = happySpecReduce_1  44# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn48
		 ([happy_var_1]
	)}

happyReduce_90 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_90 = happySpecReduce_3  45# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	happyIn49
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_91 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_91 = happySpecReduce_3  45# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn49
		 ([happy_var_1]
	)}

happyReduce_92 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_92 = happySpecReduce_3  46# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	happyIn50
		 (Lambda happy_var_1 happy_var_3
	)}}

happyReduce_93 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_93 = happySpecReduce_2  47# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	happyIn51
		 (CtorCall happy_var_1 happy_var_2
	)}}

happyReduce_94 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_94 = happySpecReduce_1  48# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TPrefix happy_var_1) -> 
	happyIn52
		 (VarVal happy_var_1
	)}

happyReduce_95 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_95 = happySpecReduce_3  48# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_2 of { (HappyWrap50 happy_var_2) -> 
	happyIn52
		 (happy_var_2
	)}

happyReduce_96 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_96 = happySpecReduce_3  48# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn52
		 (happy_var_2
	)}

happyReduce_97 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_97 = happySpecReduce_3  49# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOutTok happy_x_2 of { (TInfix happy_var_2) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	happyIn53
		 (Application (VarVal happy_var_2) [happy_var_1, happy_var_3]
	)}}}

happyReduce_98 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_98 = happySpecReduce_2  49# happyReduction_98
happyReduction_98 happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	happyIn53
		 (if null happy_var_2 then happy_var_1 else Application happy_var_1 happy_var_2
	)}}

happyReduce_99 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_99 = happySpecReduce_1  50# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOutTok happy_x_1 of { (THole happy_var_1) -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_100 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_100 = happySpecReduce_1  50# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TSmall happy_var_1) -> 
	happyIn54
		 (VarVal happy_var_1
	)}

happyReduce_101 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_101 = happySpecReduce_3  50# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	happyIn54
		 (happy_var_2
	)}

happyReduce_102 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_102 = happySpecReduce_1  51# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TValue happy_var_1) -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_103 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_103 = happySpecReduce_1  51# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_104 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_104 = happySpecReduce_1  51# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_105 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_105 = happySpecReduce_3  52# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn56
		 (mkTuple (reverse happy_var_2)
	)}

happyReduce_106 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_106 = happySpecReduce_3  53# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn57
		 ((happy_var_3:happy_var_1)
	)}}

happyReduce_107 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_107 = happySpecReduce_3  53# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn57
		 ([happy_var_3, happy_var_1]
	)}}

happyReduce_108 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_108 = happySpecReduce_2  54# happyReduction_108
happyReduction_108 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TBig happy_var_1) -> 
	case happyOut59 happy_x_2 of { (HappyWrap59 happy_var_2) -> 
	happyIn58
		 (CtorCall happy_var_1 happy_var_2
	)}}

happyReduce_109 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_109 = happySpecReduce_0  55# happyReduction_109
happyReduction_109  =  happyIn59
		 ([]
	)

happyReduce_110 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_110 = happySpecReduce_2  55# happyReduction_110
happyReduction_110 happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut54 happy_x_2 of { (HappyWrap54 happy_var_2) -> 
	happyIn59
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_111 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_111 = happySpecReduce_1  56# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_112 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_112 = happySpecReduce_1  56# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_113 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_113 = happySpecReduce_1  56# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_114 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_114 = happySpecReduce_1  56# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_115 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_115 = happySpecReduce_3  57# happyReduction_115
happyReduction_115 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_2 of { (HappyWrap62 happy_var_2) -> 
	happyIn61
		 (happy_var_2
	)}

happyReduce_116 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_116 = happySpecReduce_1  58# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	happyIn62
		 (reverse happy_var_1
	)}

happyReduce_117 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_117 = happySpecReduce_0  59# happyReduction_117
happyReduction_117  =  happyIn63
		 ([]
	)

happyReduce_118 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_118 = happySpecReduce_2  59# happyReduction_118
happyReduction_118 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	case happyOut60 happy_x_2 of { (HappyWrap60 happy_var_2) -> 
	happyIn63
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_119 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_119 = happySpecReduce_2  60# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_2 of { (HappyWrap60 happy_var_2) -> 
	happyIn64
		 ([happy_var_2]
	)}

happyReduce_120 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_120 = happySpecReduce_1  60# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn64
		 (happy_var_1
	)}

happyReduce_121 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_121 = happySpecReduce_1  61# happyReduction_121
happyReduction_121 happy_x_1
	 =  happyIn65
		 (NullStmt
	)

happyReduce_122 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_122 = happySpecReduce_1  62# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn66
		 (happy_var_1
	)}

happyReduce_123 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_123 = happySpecReduce_1  62# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	happyIn66
		 (happy_var_1
	)}

happyReduce_124 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_124 = happySpecReduce_2  62# happyReduction_124
happyReduction_124 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn66
		 (ValStmt happy_var_1
	)}

happyReduce_125 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_125 = happySpecReduce_1  62# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn66
		 (happy_var_1
	)}

happyReduce_126 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_126 = happySpecReduce_2  63# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  happyIn67
		 (Break
	)

happyReduce_127 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_127 = happySpecReduce_2  63# happyReduction_127
happyReduction_127 happy_x_2
	happy_x_1
	 =  happyIn67
		 (Continue
	)

happyReduce_128 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_128 = happySpecReduce_3  63# happyReduction_128
happyReduction_128 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn67
		 (Return happy_var_2
	)}

happyReduce_129 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_129 = happySpecReduce_1  64# happyReduction_129
happyReduction_129 happy_x_1
	 =  case happyOut70 happy_x_1 of { (HappyWrap70 happy_var_1) -> 
	happyIn68
		 (happy_var_1
	)}

happyReduce_130 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_130 = happySpecReduce_1  64# happyReduction_130
happyReduction_130 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn68
		 (happy_var_1
	)}

happyReduce_131 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_131 = happySpecReduce_1  65# happyReduction_131
happyReduction_131 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn69
		 ([happy_var_1]
	)}

happyReduce_132 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_132 = happySpecReduce_1  65# happyReduction_132
happyReduction_132 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn69
		 (happy_var_1
	)}

happyReduce_133 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_133 = happyReduce 5# 66# happyReduction_133
happyReduction_133 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	case happyOut69 happy_x_5 of { (HappyWrap69 happy_var_5) -> 
	happyIn70
		 (IfElse happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_134 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_134 = happySpecReduce_3  66# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	happyIn70
		 (IfElse happy_var_2 happy_var_3 []
	)}}

happyReduce_135 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_135 = happyReduce 5# 67# happyReduction_135
happyReduction_135 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut72 happy_x_4 of { (HappyWrap72 happy_var_4) -> 
	happyIn71
		 (Match happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_136 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_136 = happySpecReduce_1  68# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	happyIn72
		 (reverse happy_var_1
	)}

happyReduce_137 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_137 = happySpecReduce_2  69# happyReduction_137
happyReduction_137 happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	happyIn73
		 ((happy_var_2:happy_var_1)
	)}}

happyReduce_138 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_138 = happySpecReduce_1  69# happyReduction_138
happyReduction_138 happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn73
		 ([happy_var_1]
	)}

happyReduce_139 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_139 = happySpecReduce_2  70# happyReduction_139
happyReduction_139 happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	happyIn74
		 ((happy_var_1, happy_var_2)
	)}}

happyReduce_140 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_140 = happyReduce 7# 71# happyReduction_140
happyReduction_140 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	case happyOut66 happy_x_4 of { (HappyWrap66 happy_var_4) -> 
	case happyOut66 happy_x_5 of { (HappyWrap66 happy_var_5) -> 
	case happyOut69 happy_x_7 of { (HappyWrap69 happy_var_7) -> 
	happyIn75
		 (Loop happy_var_3 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_141 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_141 = happySpecReduce_3  71# happyReduction_141
happyReduction_141 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	happyIn75
		 (whileLoop happy_var_2 happy_var_3
	)}}

happyReduce_142 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_142 = happySpecReduce_2  71# happyReduction_142
happyReduction_142 happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_2 of { (HappyWrap61 happy_var_2) -> 
	happyIn75
		 (foreverLoop happy_var_2
	)}

happyReduce_143 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_143 = happyReduce 7# 72# happyReduction_143
happyReduction_143 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TSmall happy_var_3) -> 
	case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	case happyOut47 happy_x_6 of { (HappyWrap47 happy_var_6) -> 
	happyIn76
		 (NewVar happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_144 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_144 = happyReduce 6# 72# happyReduction_144
happyReduction_144 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TSmall happy_var_3) -> 
	case happyOut47 happy_x_5 of { (HappyWrap47 happy_var_5) -> 
	happyIn76
		 (NewVar happy_var_2 happy_var_3 Delayed happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_145 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )
happyReduce_145 = happyReduce 4# 73# happyReduction_145
happyReduction_145 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TSmall happy_var_1) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	happyIn77
		 (Reassignment happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TEOF -> happyDoAction 40# tk action sts stk;
	TPure -> cont 1#;
	TImpure -> cont 2#;
	TLet -> cont 3#;
	TMut -> cont 4#;
	TIntern -> cont 5#;
	TExtern -> cont 6#;
	TImport -> cont 7#;
	TUsing -> cont 8#;
	TReturn -> cont 9#;
	TIf -> cont 10#;
	TElse -> cont 11#;
	TMatch -> cont 12#;
	TLoop -> cont 13#;
	TBreak -> cont 14#;
	TContinue -> cont 15#;
	TImpl -> cont 16#;
	TTrait -> cont 17#;
	TData -> cont 18#;
	TEq -> cont 19#;
	TColon -> cont 20#;
	TSemi -> cont 21#;
	TPipe -> cont 22#;
	TArrow -> cont 23#;
	TEqArrow -> cont 24#;
	TComma -> cont 25#;
	TLParen -> cont 26#;
	TRParen -> cont 27#;
	TLBrace -> cont 28#;
	TRBrace -> cont 29#;
	TLBracket -> cont 30#;
	TRBracket -> cont 31#;
	TLAngle -> cont 32#;
	TRAngle -> cont 33#;
	TValue happy_dollar_dollar -> cont 34#;
	TBig happy_dollar_dollar -> cont 35#;
	TSmall happy_dollar_dollar -> cont 36#;
	TInfix happy_dollar_dollar -> cont 37#;
	TPrefix happy_dollar_dollar -> cont 38#;
	THole happy_dollar_dollar -> cont 39#;
	_ -> happyError' (tk, [])
	})

happyError_ explist 40# tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyParse :: () => Happy_GHC_Exts.Int# -> Alex (HappyAbsSyn )

happyNewToken :: () => Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )

happyDoAction :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn )

happyReduceArr :: () => Happy_Data_Array.Array Prelude.Int (Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Alex (HappyAbsSyn ))

happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
rose = happySomeParser where
 happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (let {(HappyWrap4 x') = happyOut4 x} in x'))

happySeq = happyDoSeq


parseError :: Token -> Alex a
parseError = lexError . terse
-- parseError token = do
--     pos <- getLexerPos
--     alexError $
--         Red|+"\nError parsing token ("*|pos|*"):\n"+|
--         Yellow|*|token|*"\n"


whileLoop :: Value -> Body -> Stmt
whileLoop stmt = Loop NullStmt (ValStmt stmt) NullStmt

foreverLoop :: Body -> Stmt
foreverLoop  = Loop NullStmt NullStmt NullStmt

mkTuple :: [Value] -> Value
mkTuple vals = Tuple
    (listArray (0, length vals)
    (reverse vals))

mkArray :: [Value] -> Value
mkArray vals = Array
    (listArray (0, length vals)
    (reverse vals))
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList

































happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = (happyTrace ("state: " ++ show (Happy_GHC_Exts.I# (st)) ++ 
                      ",\ttoken: " ++ show (Happy_GHC_Exts.I# (i)) ++
                      ",\taction: ")) $
          case action of
                0#           -> (happyTrace ("fail.\n")) $
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> (happyTrace ("accept.\n")) $
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> (happyTrace ("reduce (rule " ++ show rule
                                                               ++ ")")) $
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> (happyTrace ("shift, enter state "
                                                 ++ show (Happy_GHC_Exts.I# (new_state))
                                                 ++ "\n")) $
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (Happy_GHC_Exts.I# (new_state)) ++ "\n")) $
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
