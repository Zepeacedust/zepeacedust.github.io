---
date: 11/5/2025
title: "How it all works 2: Electric boogaloo"
abstract: I rewrote the whole thing so I don't want to punch my monitor when I want to add anything.
tags:
    - meta
    - coding
---
I was working on some extension to the blog compiler, when I started to get fed up with the absolute mess the codebase was.
After one thought of "This is stupid, why am I doing it this way?" too many, I realized that I knew why I was doing it that way.
I wrote most of it mostly asleep, and never revised anything.

<aside>
Turns out that treating files and directories the same way leads to a whole lot of edge case handling.
Probably half of the functions I was writing errored if I passed a directory into it, this was not well documented at all.
</aside>

At the root of the issue was the simple fact that individual pages were being treated as files first, and pages second.
This version seeks to remedy that by replacing the central folder datastructure with one focused on posts.

```haskell
data BlogPost = BlogPost {
  fileName :: String,
  postTitle :: T.Text,
  postTags :: [T.Text],
  postDate :: UTCTime,
  postContent :: Pandoc,
  desiredTemplate :: Maybe T.Text
  }
```

While this does duplicate a decent bit of data, I kind of don't care since accessing metadata is finnicky and can fail.
Having the interesting metadata in a structured format, where I can reason about it within the structure of haskells type system.
While doing this I also decided that only having a hard coded template for posts and indices is constraining.
Now I can have a clean look for the respectable projects, and still have space for a parchment background for when I need to feel like a wizard.

Another major usability improvement is pulling the configuration out of hardcoded constants and into a seperate json file.
The structure file for the blog currently looks like this.

```json
{
    "templateFolder":"templates",
    "sourceFolder":"blogsrc",
    "destinationFolder":".",
    "defaultTemplate" : "template.html",
    "indices":[
        {
            "indexTemplate":"indexTemp.html",
            "indexFolder":"blogsrc/blog"
        }
    ]
}
```

Which is then transformed parsed into the following datastructures using aeson.

```haskell
data IndexDescriptor = IndexDescriptor {
  indexFolder :: String,
  indexTemplate :: T.Text
  }
$(deriveJSON defaultOptions ''IndexDescriptor)

data Config = Config {
  sourceFolder :: String,
  destinationFolder :: String,
  templateFolder :: String,
  defaultTemplate :: T.Text,
  indices :: [IndexDescriptor]
  }
$(deriveJSON defaultOptions ''Config)
```

[Aeson](https://hackage.haskell.org/package/aeson) is a json parsing library, and it can parse a json file into a haskell datastructure without any extra work.
This config object is then passed through the parts that need it, I might want to make a more elegant solution, but I don't know if one really exists.
Now I don't need to rebuild the entire program when I want to change where I want the index to be, why didn't I think of this before :).

The new blog compiler is [currently hosted at github](https://github.com/Zepeacedust/Blog-Compiler), I figured that having the content and the compiler so tightly coupled can only go poorly.
That was also partially motivated by another project where I needed to make a static site, so I wanted to tweak the tooling to support that.

I find it quite funny that I have done almost as many updates to the backend as I have to the actual content of the blog, I'll say that I want to make more.
Probably won't though, and you can't make me.
