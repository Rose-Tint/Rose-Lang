module Builder.Base (
    module B,
    module Builder.Base,
) where

import Builder.CmdLine as B
import Builder.Internal
import qualified Builder.Internal as B (
    buildM,
    liftBuild,
    (<#>),
    getModule,
    )
import Builder.IO as B
import Builder.Output as B
import Builder.State


getFilePath :: BuilderT m FilePath
getFilePath = stFile <$!> getState

getModuleName :: BuilderT m String
getModuleName = stModule <$!> getState

getBaseBuildDir :: BuilderT m FilePath


