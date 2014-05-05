{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module WebPage.Generate.Context (
  TemplateApplication,
  mainTemplate,
) where

import Data.List (find)
import Data.Monoid ((<>))
import System.FilePath
import Hakyll

import WebPage.Pubs

type TemplateApplication = String -> Item String -> Compiler (Item String)

-- | Apply a template to a page of a given name.
applyTemplateTo :: Identifier -> TemplateApplication
applyTemplateTo template =
    \ _ item -> do
      context <- getContext
      applyAsTemplate context item
        >>= loadAndApplyTemplate template context
        >>= relativizeUrls

-- | Apply the main template to a page of a given name.
mainTemplate :: TemplateApplication
mainTemplate = applyTemplateTo "templates/base.html"

getContext :: Compiler (Context String)
getContext = do
  pubContext <- getPubContext
  blurbContext <- getBlurbContext
  return $ pubContext <> blurbContext

-- | Makes the contents of the blurbs directory available as template fields.
getBlurbContext :: Compiler (Context String)
getBlurbContext = do
    loadAll "blurbs/*"
      >>= return . foldr (<>) baseContext . map item
  where item (Item iD body) = constField (takeBaseName (toFilePath iD)) body

-- ** Publication context

-- | Adds the PDF link if the file is present.
linkPdf :: [Item CopyFile] -> Paper -> Paper
linkPdf fs p
    | Just _ <- lookupItem pdf fs = p `setPdfLink` ("/" ++ pdf)
    | otherwise = p
  where
    pdf = "papers/" ++ _key p ++ ".pdf"

-- | Add the abstract if the corresponding file is present.
addAbstract :: [Item String] -> Paper -> Paper
addAbstract fs p
    | Just i <- lookupItem abst fs = p `setAbstract` itemBody i
    | otherwise = p
  where
    abst = "papers/" ++ _key p ++ ".abstract.md"

-- | Build a list field of publications.
pubListField :: String -> [Paper] -> Context String
pubListField iid = listField iid baseContext . mapM (makeItem . pubStr)

-- | Create a field for a publications.
pubFields :: Paper -> Context a
pubFields p = constField (_key p) (pubStr p)

getPubContext :: Compiler (Context String)
getPubContext = do
  isindex <- fmap (== "pages/index.html") getUnderlying
  pdfs <- loadAll "papers/*.pdf"
  txts <- loadAll "papers/*.abstract.md"
  let pubs = map (addAbstract txts . linkPdf pdfs) allPubs
  let pubListContext =
          pubListField "pubs"     pubs
          <> pubListField "journals" (shortenIf isindex $ ofKind Journal pubs)
          <> pubListField "chapters" (shortenIf isindex $ ofKind Chapter pubs)
          <> pubListField "theses"   (shortenIf isindex $ ofKind Thesis pubs)
          <> pubListField "conferences" (shortenIf isindex
                                         $ ofKinds [Conference,Workshop, Poster] pubs)
          <> pubListField "others" (shortenIf isindex $ ofKind Other pubs)
  return $ foldr (<>) pubListContext (map pubFields pubs)
    where shortenIf p = if p then take pubLength else id
          pubLength = 3

baseContext :: Context String
baseContext = defaultContext

lookupItem :: FilePath -> [Item a] -> Maybe (Item a)
lookupItem path = find ((fromFilePath path ==) . itemIdentifier)
