import Text.Pandoc
import Data.List.Split (splitWhen)
import System.FilePath (replaceExtension, takeDirectory)
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data FolderStructure = Folder String [FolderStructure] | File String deriving Show

getFolderStructure :: FilePath -> IO FolderStructure
getFolderStructure root = do
  isFile <- D.doesFileExist root
  isFolder <- D.doesDirectoryExist root
  case (isFile, isFolder) of 
    (True, False) -> return (File root)
    (False, True) -> do
      files <- D.listDirectory root
      inner <- sequence.map getFolderStructure $ map (\f -> root ++ "/" ++ f) files
      return (Folder root inner)
    (True, True) -> error (root ++ " is both Folder and Directory... What?")
    (False,False) -> error (root ++ "does not exist")

flatten :: FolderStructure -> [String]
flatten (Folder _ inner)  = concat.map flatten $ inner 
flatten (File name) = [name]

swapPrefix :: Eq a => a -> [a] ->  [a] -> [a]
swapPrefix sep new old = concat (new : [sep] :  rest) where
  tip:rest = splitWhen (==sep) old

removePrefix :: Eq a => a -> [a] -> [a]
removePrefix sep old = concat.tail $ splitWhen (== sep) old


destinationFile :: String -> String
destinationFile s = replaceExtension (removePrefix '/' s) "html"

writeMakingFile :: String -> T.Text -> IO()
writeMakingFile name t = do
  D.createDirectoryIfMissing True . takeDirectory $ name
  TIO.writeFile name t

transpile :: Template T.Text -> String -> IO ()
transpile template fileName =  do
  text <- TIO.readFile fileName
  Right out <- runIO $ do
    doc <- readMarkdown def{readerExtensions = extensionsFromList [Ext_yaml_metadata_block], readerStandalone = True} text
    writeHtml5String def{
        writerTemplate = Just template
      } doc 
  
  writeMakingFile (destinationFile fileName) out

makeTemplate :: String -> IO (Template T.Text)
makeTemplate templateName = do 
  Right temp <- runIO (getTemplate templateName)
  Right fin <- compileTemplate "" temp
  return fin

main :: IO ()
main = do
  struct <- getFolderStructure "blogsrc"
  let allFiles = flatten struct
  template <- makeTemplate "template.html"
  sequence.map (transpile template) $ allFiles
  print "all done"

