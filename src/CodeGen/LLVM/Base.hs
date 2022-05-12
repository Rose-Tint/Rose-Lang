module CodeGen.LLVM.Base where

import qualified LLVM.AST as LLVM

import CodeGen.LLVM.ToLLVM
import Builder.Internal
import Utils.String


dataLayout :: LLVM.DataLayout
dataLayout = LLVM.defaultLayout {
    endianess = LLVM.LittleEndian,
    -- these fields are set to the default
    -- bc i dont know what they mean yet :(
    -- stackAlignment = ,
    -- pointerLayouts = ,
    -- typeLayouts = ,
    -- aggregateLayouts = ,
    nativeSizes = Nothing
    }

newModule :: BuilderT m LLVM.Module
newModule = do
    fp <- stFile <$> getState
    name <- stModule <$> getState
    return $! LLVM.defaultModule {
            LLVM.moduleName = strToShortBS name,
            LLVM.moduleSourceFileName = strToShortBS fp,
            LLVM.moduleDataLayout = dataLayout,
            LLVM.moduleTargetTriple = Nothing, -- what is this?
            }
