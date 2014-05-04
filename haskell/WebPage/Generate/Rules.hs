{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules where
import Hakyll

compileRules :: Rules ()
compileRules = do
  compileTemplates
  compileMarkdown
  compileCss

compileTemplates :: Rules ()
compileTemplates =
    match "templates/*" $ compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown =
    match "teaching/*.md" $ compile pandocCompiler

compileCss :: Rules ()
compileCss =
    match "css/*" $ do
      route idRoute
      compile copyFileCompiler
