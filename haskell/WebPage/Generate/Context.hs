{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module WebPage.Generate.Context (
  TemplateApplication,
  mainTemplate,
) where

import Hakyll


type TemplateApplication = String -> Item String -> Compiler (Item String)

-- | Apply a template to a page of a given name.
applyTemplateTo :: Identifier -> TemplateApplication
applyTemplateTo template =
    \ _ item -> do
      let context = defaultContext
      applyAsTemplate context item
        >>= loadAndApplyTemplate template context
        >>= relativizeUrls

-- | Apply the main template to a page of a given name.
mainTemplate :: TemplateApplication
mainTemplate = applyTemplateTo "templates/base.html"
