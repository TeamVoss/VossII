-- | Functionality to write custom 'FlType' implementations.
module Language.FL.Plugin.Internal
  ( -- * @FlType@ definitions
    FlType (..)
    -- * Basic low-level fl types
  , Proxy (..), Ptr, Node, Redex, FlTypeExp
    -- * GC'd fl pointers to Haskell objects
  , FlPtr, module FlPtr
    -- * The low-level fl API plugin
  , module API
  ) where
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import Language.FL.Plugin.API as API hiding
  (withAPI, activateAPI, getPluginExports, getFlApi, pluginState)
import Language.FL.Plugin.FlPtr (FlPtr)
import Language.FL.Plugin.FlPtr as FlPtr hiding (FlPtr (FlPtr))
import Language.FL.Plugin.FlType (FlType (..))
import Language.FL.Plugin.Foreign.Types (Node, Redex, FlTypeExp)
