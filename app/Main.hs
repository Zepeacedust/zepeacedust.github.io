import Text.Pandoc
import Data.List (intercalate)
import Data.List.Split (splitWhen)
import System.FilePath (replaceExtension, takeDirectory)
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M 
data FolderStructure = Dir String [FolderStructure] | File String Pandoc deriving Show

getFolderStructure :: FilePath -> IO FolderStructure
getFolderStructure root = do
  isFile <- D.doesFileExist root
  isFolder <- D.doesDirectoryExist root
  case (isFile, isFolder) of 
    (True, False) -> do
      contents <- processFile root
      return (File root contents)
    (False, True) -> do
      files <- D.listDirectory root
      inner <- sequence.map getFolderStructure $ map (\f -> root ++ "/" ++ f) files
      return (Dir root inner)
    (True, True) -> error (root ++ " is both Folder and Directory... What?")
    (False,False) -> error (root ++ "does not exist")

flatten :: FolderStructure -> [(String, Pandoc)]
flatten (Dir _ inner)  = concat.map flatten $ inner 
flatten (File name content) = [(name, content)]

removePrefix :: Eq a => a -> [a] -> [a]
removePrefix sep old = intercalate [sep].tail $ splitWhen (== sep) old


destinationFile :: String -> String
destinationFile s = replaceExtension (removePrefix '/' s) "html"

writeMakingFile :: String -> T.Text -> IO()
writeMakingFile name t = do
  D.createDirectoryIfMissing True . takeDirectory $ name
  TIO.writeFile name t

processFile :: String -> IO Pandoc
processFile fileName = do
  text <- TIO.readFile fileName
  Right out <- runIO $ do
    readMarkdown def{readerExtensions = extensionsFromList [Ext_yaml_metadata_block], readerStandalone = True} text
  return out

pasteFile :: Template T.Text -> String -> Pandoc -> IO ()
pasteFile template fileName content = do
  Right out <- runIO $ do
    writeHtml5String def {writerTemplate = Just template} content
  writeMakingFile fileName out  

makeTemplate :: String -> IO (Template T.Text)
makeTemplate templateName = do 
  Right source <- runIO (getTemplate templateName)
  Right templ <- compileTemplate "" source
  return templ

dumpFiles :: Template T.Text -> FolderStructure -> IO ()
dumpFiles template (Dir _ inner) = do
  _ <- sequence.map (dumpFiles template) $ inner
  return ()
dumpFiles template (File name content) = do
  pasteFile template (destinationFile name) content

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

isDirWithName name (Dir n _) = n == name
isDirWithName _ _ = False

findDir name (Dir _ inner) = find (isDirWithName name) inner

getMeta (Pandoc meta _ ) = meta

getContent (File _ content) = content
getContent (Dir n _) = error ("Directory " ++ n ++ " has no content")

getMetaRep (File name content)=
  let original = unMeta.getMeta $  content
      path     = M.singleton (T.pack "path") (MetaString (T.pack.destinationFile $ name))
   in MetaMap(M.union original path)

makeIndex (Dir name inner) = do
  let indexName = destinationFile (name++"/index.html")
  let postList = MetaList(map getMetaRep inner)
      newMeta = Meta (M.singleton (T.pack "post") postList)
  template <- makeTemplate "templates/indexTemp.html"
  pasteFile template indexName(Pandoc newMeta [])

main :: IO ()
main = do
  struct <- getFolderStructure "blogsrc"
  template <- makeTemplate "templates/template.html"
  dumpFiles template struct
  let Just blogFolder = findDir "blogsrc/blog" struct
  makeIndex blogFolder
  putStrLn "all done"

