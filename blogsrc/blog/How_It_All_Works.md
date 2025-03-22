---
date: 22/3/2025
title: How it all works
abstract: A quick overview of how the blog is generated.
---

The blog is generated with a custom script that takes a bunch of markdown files and turns them into html via pandoc.
It all started when I saw a some blog espousing the virtues of static websites and thought "I could do that".

So I had a bunch of issues with cabal and arch really not liking each other, and then started writing code pretty early in the evening.

Let's just go over the whole deal.


To copy the folder structure and work with it neatly, I have a folder datastructure.

``` haskell
data FolderStructure = Dir String [FolderStructure] | File String Pandoc deriving Show
```

And every good datastructure needs to have a way to generate it

```haskell
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
```

It's a bit clunky, but it's good enough.

Then I need a bit of filename manipulation that was not present in the libraries I was using, so I just wrote a few.

```haskell
removePrefix :: Eq a => a -> [a] -> [a]
removePrefix sep old = intercalate [sep].tail $ splitWhen (== sep) old

destinationFile :: String -> String
destinationFile s = replaceExtension (removePrefix '/' s) "html"
```

I want a way to write a file and create all its parent directories in one go.

```haskell
writeMakingFile :: String -> T.Text -> IO()
writeMakingFile name t = do
  D.createDirectoryIfMissing True . takeDirectory $ name
  TIO.writeFile name t
```

Then comes reading and writing the files themselves, this is mostly pandoc with a slight personal touch to wrap it in something that does what I want.

```haskell
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
```

Templates were a real piece of work, until I read the fucking manual(tm), I thought that compileTemplate was a PandocMonad but it's a TemplateMonad, which is either IO or Id.
Don't make my mistakes kids.

This is all wrapped in a single function to isolate all of work somewhere that I can reuse it easily.

```haskell
makeTemplate :: String -> IO (Template T.Text)
makeTemplate templateName = do 
  Right source <- runIO (getTemplate templateName)
  Right templ <- compileTemplate "" source
  return templ
```

And putting all those together, I have a function that can take a whole folder and turn it into nice html with minimal manual intervention.

```haskell
dumpFiles :: Template T.Text -> FolderStructure -> IO ()
dumpFiles template (Dir _ inner) = do
  _ <- sequence.map (dumpFiles template) $ inner
  return ()
dumpFiles template (File name content) = do
  pasteFile template (destinationFile name) content
```

But a blog that I can't just dump things into and make available is no blog at all, if I need to write links to all of my rants manually why even bother?

So I set to work writing an index generator, starting with finding the directory to make an index of.

```haskell
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs


isDirWithName :: String -> FolderStructure -> Bool
isDirWithName name (Dir n _) = n == name
isDirWithName _ _ = False

findDir :: String -> FolderStructure -> Maybe FolderStructure
findDir name (Dir _ inner) = find (isDirWithName name) inner
findDir _ _ = error "Not a directory"
```

That was mostly busywork to make the interesting stuff work, and the interesting stuff is to scrape the metadata of all the files in the folder and paste them into a template.

```haskell
getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _ ) = meta

getMetaRep :: FolderStructure ->  MetaValue
getMetaRep (File name content) =
  let original = unMeta.getMeta $  content
      path     = M.singleton (T.pack "path") (MetaString (T.pack.destinationFile $ name))
   in MetaMap(M.union original path)
getMetaRep _ = error "Directory has no metadata"
```
With the scraping done, I just need to dump the scraped metadata into a list and pipe that list into the template.

```haskell
makeIndex :: FolderStructure -> IO ()
makeIndex (Dir name inner) = do
  let indexName = destinationFile (name++"/index.html")
  let postList = MetaList(map getMetaRep inner)
      newMeta = Meta (M.singleton (T.pack "post") postList)
  template <- makeTemplate "templates/indexTemp.html"
  pasteFile template indexName(Pandoc newMeta [])
```
And the main function is just taping all that logic together, it's nice enough but I will probably bastardize it with more modifications down the line someday.
```haskell
main :: IO ()
main = do
  struct <- getFolderStructure "blogsrc"
  template <- makeTemplate "templates/template.html"
  dumpFiles template struct
  let Just blogFolder = findDir "blogsrc/blog" struct
  makeIndex blogFolder
  putStrLn "all done"
```
This is all available on the [github](https://github.com/zepeacedust/zepeacedust.github.io)
