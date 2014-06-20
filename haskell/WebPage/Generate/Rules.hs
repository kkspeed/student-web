{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules where
import System.FilePath
import Hakyll

import WebPage.Generate.Context

compileRules :: Rules ()
compileRules = do
  compileTemplates
  compileMarkdown
  compileCss
  compileImage
  buildPages
  copyPapers
  loadAbstracts
  copyCV

compileTemplates :: Rules ()
compileTemplates =
    match "templates/*" $ compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown =
    match ("blurbs/*.md" .||. "teaching/*.md") $ compile pandocCompiler

compileCss :: Rules ()
compileCss =
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

compileImage :: Rules ()
compileImage =
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

buildPages :: Rules ()
buildPages = do
    match "pages/*" $ do
      route (customRoute (flip addExtension "html" . takeBaseName . toFilePath))
      compilePage mainTemplate

compilePage :: TemplateApplication -> Rules ()
compilePage apply = compile $ do
    path <- fmap toFilePath getUnderlying
    let content = case takeExtension path of
                    ".html" -> getResourceBody
                    _       -> error ("Unexpected file type: " ++ path)
    content >>= apply (takeBaseName path)

copyPapers :: Rules ()
copyPapers =
  match "papers/*.pdf" $ do
    route   idRoute
    compile copyFileCompiler

copyCV :: Rules ()
copyCV =
  match "cv/*.pdf" $ do
    route   idRoute
    compile copyFileCompiler

loadAbstracts :: Rules ()
loadAbstracts =
  match "papers/*.abstract.md" $
    compile pandocCompiler
