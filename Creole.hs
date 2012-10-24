{-# LANGUAGE NoMonomorphismRestriction #-}

module Creole where

import Text.ParserCombinators.Parsec


restOfLine = many $ noneOf "\n"

nakedLink = do
  string "http://"
  target <- many $ noneOf " \n"
  rest <- lineContent
  return $ "<a href=\"http://" ++ target ++ "\">" ++ target ++ "</a>" ++ rest
  
textInBold = do
  string "**"
  boldText <- many $ noneOf "*"
  string "**"
  rest <- lineContent
  return $ "<strong>" ++ boldText ++ "</strong>" ++ rest

textInItalics = do
  string "//"
  italicsText <- many $ noneOf "/"
  string "//"
  rest <- lineContent
  return $ "<em>" ++ italicsText ++ "</em>" ++ rest
  
-- todo: single ] in links
unnamedLink :: Parser String
unnamedLink = do
  string "[["
  target <- many $ noneOf "]"
  string "]]"
  rest <- lineContent
  return $ "<a href=\"" ++ target ++ "\">" ++ target ++ "</a>" ++ rest
  
namedLink :: Parser String
namedLink = do
  string "[["
  target <- many $ noneOf "]|"
  string "|"
  name <- many $ noneOf "]"
  string "]]"
  rest <- lineContent
  return $ "<a href=\"" ++ target ++ "\">" ++ name ++ "</a>" ++ rest
  
  
lineBreak :: Parser String
lineBreak = do
  string "\\\\"
  rest <- lineContent
  return $ "<br>" ++ rest

-- text, links, bold/italic (things inside paragraphs)
lineContent :: Parser String
lineContent =
  -- TODO: Single punctuation characters (,.?!:;"') at the end of URLs should not be considered part of the URL.
      try nakedLink
  <|> try textInBold
  <|> try textInItalics
  <|> try namedLink
  <|> try unnamedLink
  <|> try lineBreak
  <|> try (do
              char <- noneOf "\n"
              rest <- lineContent
              return $ char : rest)
  <|> return ""
  
noTrailingEquals :: Parser String
noTrailingEquals = 
  try (do
          text <- many1 $ noneOf "=\n"
          rest <- noTrailingEquals
          return $ text ++ rest)
  <|> try (do
          equals <- many1 $ oneOf "="
          afterEquals <- noTrailingEquals
          case afterEquals of
            [] -> return ""
            _ -> return $ equals ++ afterEquals)
  <|> return ""
      
heading1 = do
  string "="
  spaces
  heading <- noTrailingEquals
  newline
  return $ "<h1>" ++ heading ++ "</h1>"
heading2 = do
  string "=="
  spaces
  heading <- noTrailingEquals
  newline
  return $ "<h2>" ++ heading ++ "</h2>"
heading3 = do
  string "==="
  spaces
  heading <- noTrailingEquals
  newline
  return $ "<h3>" ++ heading ++ "</h3>"
heading4 = do
  string "===="
  spaces
  heading <- noTrailingEquals
  newline
  return $ "<h4>" ++ heading ++ "</h4>"
  
paragraph :: Parser String
paragraph = do
  paragraph <- lineContent
  string "\n\n"
  return $ "<p>" ++ paragraph ++ "</p>"
  
nowiki = do
  string "{{{"
  nowiki <- many (try (string "}" >> noneOf "}") <|> try (string "}}" >> noneOf "}") <|> noneOf "}")
  string "}}}"
  return $ "<pre>" ++ nowiki ++ "</pre>"
  
  
-- todo: space after asterisk is optional
unorderedListItem :: Parser (Maybe (Int, String))
unorderedListItem =
  try (do
          indentLevel <- many1 $ oneOf "*"
          spaces
          item <- lineContent
          newline
          return $ Just (length indentLevel, item)
      )
  <|> return Nothing

-- todo: leading whitespace
unorderedListItems :: Parser [(Int, String)]
unorderedListItems = do
  item <- unorderedListItem
  case item of
    Just (indentLevel, line) -> do
      rest <- unorderedListItems
      return $ (indentLevel, line) : rest
    Nothing -> return []
    
-- todo: badly nested lists
listToHtml :: [(Int, String)] -> String
listToHtml items =
  listToHtml' 0 items
  where
    listToHtml' currentIndent listItems@((indent, line):items)
      | currentIndent < indent = "<ul><li>" ++ line ++ (listToHtml' (currentIndent + 1) items)
      | currentIndent == indent = "</li><li>" ++ line ++ (listToHtml' currentIndent items)
      | currentIndent > indent = "</li></ul>" ++ listToHtml' (currentIndent - 1) items
    listToHtml' currentIndent [] = 
      if currentIndent == 0 then ""
      else "</li></ul>" ++ listToHtml' (currentIndent - 1) []

unorderedList :: Parser String
unorderedList = do
  items <- unorderedListItems
  newline
  return (listToHtml items)

-- either a paragraph or a heading
-- TODO: headings may end with matching == too
-- everything ends with a newline, except paragraphs, which require two
block :: Parser String
block = 
  -- need to match subsubheadings first
      try heading4
  <|> try heading3
  <|> try heading2
  <|> try heading1
  <|> try unorderedList
  <|> try paragraph
  <|> try nowiki
  
-- parse Creole 1.0 source and return HTML
-- requires a trailing newline
creole :: Parser String
creole =
  try (do
          block' <- block
          rest <- creole
          return $ block' ++ "\n" ++ rest)
  <|> return ""
  
out2 = parseTest creole "{{{ foo }}}"
out3 = parseTest creole "some **bold** text and //italics// too!\\\\newline\n\n"
out4 = parseTest creole "*foo\n*bar\n\n"
out5 = parseTest creole "*foo\n*bar **baz**\n\n"
