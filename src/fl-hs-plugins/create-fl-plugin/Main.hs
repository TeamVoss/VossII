{-# LANGUAGE CPP, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Main where
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import System.Directory
import System.FilePath
import System.IO
import Text.Mustache
import Text.RawString.QQ
import Paths_fl_plugins

mainTemplate :: String
mainTemplate = "main.mustache"

hsbracketTemplate :: String
hsbracketTemplate = "hsbracket.mustache"

cabalTemplate :: String
cabalTemplate = "cabal.mustache"

makefileTemplate :: String
makefileTemplate = "Makefile.mustache"

templates :: [(FilePath, Text)]
templates = map (fmap decodeUtf8) $(embedDir "templates")

#ifdef DEFAULT_PLUGIN_DIR
defaultPluginDir :: Maybe String
defaultPluginDir
  | null dir  = Nothing
  | otherwise = Just dir
  where dir = [r|DEFAULT_PLUGIN_DIR|]
#else
defaultPluginDir = Nothing
#endif

getPluginDir :: IO FilePath
getPluginDir = do
  case defaultPluginDir of
    Just dir -> return dir
    Nothing  -> return "."

data PluginInfo = PluginInfo
  { pluginName :: String
  , pluginAuthor :: String
  , pluginEmail :: String
  , pluginBaseVersion :: String
  , pluginFlPluginsVersion :: String
  , pluginInstallDirectory :: FilePath
  }

instance ToMustache PluginInfo where
  toMustache p = object
    [ "plugin-name"        ~> pluginName p
    , "author"             ~> pluginAuthor p
    , "email"              ~> pluginEmail p
    , "base-version"       ~> pluginBaseVersion p
    , "fl-plugins-version" ~> pluginFlPluginsVersion p
    , "install-dir"        ~> pluginInstallDirectory p
    ]

ask :: Maybe String -> String -> IO String
ask mdef q = do
  case mdef of
    Just def -> putStr (q ++ " [" ++ def ++ "] ")
    _        -> putStr (q ++ " ")
  hFlush stdout
  ln <- getLine
  if null ln
    then maybe (ask mdef q) pure mdef
    else return ln

main :: IO ()
main = do
  currentDir <- takeBaseName <$> getCurrentDirectory
  pluginDir <- getPluginDir
  libName <- ask (Just currentDir) "Plugin name?"
  author <- ask Nothing "Author name?"
  email <- ask Nothing "Author email?"
  installDir <- ask (Just pluginDir) "Target directory for `make install'?"
  createPluginSkeleton $ PluginInfo
    { pluginName = libName
    , pluginAuthor = author
    , pluginEmail = email
    , pluginBaseVersion = VERSION_base
    , pluginFlPluginsVersion = VERSION_fl_plugins
    , pluginInstallDirectory = installDir
    }

createPluginSkeleton :: PluginInfo -> IO ()
createPluginSkeleton p = do
  createDirectories
  writeCabalFile p
  writeMakeFile p
  writeSourceFiles p

writeTemplateFile :: PluginInfo -> String -> FilePath -> IO ()
writeTemplateFile p name outfile =
    Text.writeFile outfile (substitute template p)
  where
    (templateName, templateText) = head [t | t <- templates, fst t == name]
    Right template = compileTemplate templateName templateText

writeCabalFile :: PluginInfo -> IO ()
writeCabalFile p = do
  writeTemplateFile p cabalTemplate (pluginName p <.> "cabal")

writeMakeFile :: PluginInfo -> IO ()
writeMakeFile p = do
  writeTemplateFile p makefileTemplate "Makefile"

writeSourceFiles :: PluginInfo -> IO ()
writeSourceFiles p = do
  writeTemplateFile p hsbracketTemplate ("csrc" </> "hsbracket.c")
  writeTemplateFile p mainTemplate ("src" </> "Main.hs")

createDirectories :: IO ()
createDirectories = mapM_ (createDirectoryIfMissing False) ["csrc", "src"]
