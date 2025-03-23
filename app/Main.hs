import Text.Pandoc
import Data.List (intercalate, sortBy, nub)
import Data.List.Split (splitWhen)
import System.FilePath (replaceExtension, takeDirectory)
import Data.Ord
import Data.Time
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
    (True, True) -> error (root ++ " is both File and Directory... What?")
    (False,False) -> error (root ++ "does not exist")

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
    readMarkdown def{readerExtensions = extensionsFromList [Ext_yaml_metadata_block, Ext_backtick_code_blocks, Ext_grid_tables, Ext_tex_math_dollars], readerStandalone = True} text
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

isDirWithName :: String -> FolderStructure -> Bool
isDirWithName name (Dir n _) = n == name
isDirWithName _ _ = False

findDir :: String -> FolderStructure -> Maybe FolderStructure
findDir name (Dir _ inner) = find (isDirWithName name) inner
findDir _ _ = error "Not a directory"

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _ ) = meta

getMetaRep :: FolderStructure ->  MetaValue
getMetaRep (File name content) =
  let original = unMeta.getMeta $  content
      path     = M.singleton (T.pack "path") (MetaString (T.pack.destinationFile $ name))
   in MetaMap(M.union original path)
getMetaRep _ = error "Directory has no metadata"

getContent :: FolderStructure -> Pandoc
getContent (File _ content) = content
getContent (Dir name _) = error (name ++" is a Directory not file")

--getDate :: FolderStructure -> T.Text
getDate (File name content) = date where
  meta = getMeta content
  result = lookupMeta (T.pack "date") meta
  Just(MetaInlines[Str dateString]) = result -- fuck ugly but it works
  date = parseTimeOrError True defaultTimeLocale "%-d/%-m/%Y" (T.unpack dateString) :: UTCTime

sortByDate :: [FolderStructure] ->[FolderStructure]
sortByDate = sortBy (comparing getDate) 

--getTags :: FolderStructure -> [T.Text]
getTags file = tags where
  meta = getMeta.getContent $ file
  Just (MetaList metaTags) = lookupMeta (T.pack "tags") meta -- This is just filty
  tags = map (\(MetaInlines [Str tag]) -> tag) metaTags -- This is too, but as long as I follow the format it will still work

getAllTags :: [FolderStructure] -> [T.Text] 
getAllTags fs = nub.concat.map getTags$ fs

hasTag :: T.Text -> FolderStructure -> Bool
hasTag tag file = elem tag (getTags file)

makeIndex :: FolderStructure -> IO ()
makeIndex (Dir name inner) = do
  makeIndexFromFiles (name++"/index.html") inner -- full index
  let tags = getAllTags inner
  sequence (map (\tag -> makeIndexFromFiles (name++"/index_"++(T.unpack tag)) (filter (hasTag tag) inner)) tags) -- tag indices
  return ()

makeIndexFromFiles :: String -> [FolderStructure] -> IO ()
makeIndexFromFiles name inner = do
  let indexName = destinationFile (name)
  let postList = MetaList(map getMetaRep (reverse.sortByDate$ inner))
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

