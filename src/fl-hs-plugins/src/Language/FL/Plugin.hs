module Language.FL.Plugin
  ( FlExport, FlType
  , FlInit, Text
  , Plugin (..), Fixity (..), Associativity (..), Export
  , flInit, export, fixity
  , FlError (..)
  ) where
import Data.Text (Text)
import Language.FL.Plugin.API (FlError (..))
import Language.FL.Plugin.Exports (export)
import Language.FL.Plugin.Exports.Types
import Language.FL.Plugin.FlExport.Instances ()
import Language.FL.Plugin.FlExport
import Language.FL.Plugin.FlExport.Instances ()
import Language.FL.Plugin.FlType
import Language.FL.Plugin.FlType.FunctionInstances ()
import Language.FL.Plugin.FlType.Instances ()
import Language.FL.Plugin.FlType.TupleInstances ()
import Language.FL.Plugin.Foreign (FlInit, flInit)
