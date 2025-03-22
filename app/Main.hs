import Text.Pandoc
import Data.List (intercalate)
import Data.List.Split (splitWhen)
import System.FilePath (replaceExtension, takeDirectory)
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data FolderStructure = Dir String [FolderStructure] | File String deriving Show

getFolderStructure :: FilePath -> IO FolderStructure
getFolderStructure root = do
  isFile <- D.doesFileExist root
  isFolder <- D.doesDirectoryExist root
  case (isFile, isFolder) of 
    (True, False) -> return (File root)
    (False, True) -> do
      files <- D.listDirectory root
      inner <- sequence.map getFolderStructure $ map (\f -> root ++ "/" ++ f) files
      return (Dir root inner)
    (True, True) -> error (root ++ " is both Folder and Directory... What?")
    (False,False) -> error (root ++ "does not exist")

flatten :: FolderStructure -> [String]
flatten (Dir _ inner)  = concat.map flatten $ inner 
flatten (File name) = [name]

removePrefix :: Eq a => a -> [a] -> [a]
removePrefix sep old = intercalate [sep].tail $ splitWhen (== sep) old


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
  Right source <- runIO (getTemplate templateName)
  Right templ <- compileTemplate "" source
  return templ

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

isDirWithName name (Dir n _) = n == name
isDirWithName _ (File _) = False

findDir name (Dir _ inner) = find (isDirWithName name) inner

makeIndex tree = do
  print "TeeHee"

main :: IO ()
main = do
  struct <- getFolderStructure "blogsrc"
  let allFiles = flatten struct
  template <- makeTemplate "templates/template.html"
  _ <- sequence.map (transpile template) $ allFiles
  let blogFolder = findDir "blog" struct
  makeIndex blogFolder
  putStrLn "all done"

