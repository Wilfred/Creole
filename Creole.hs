{-# LANGUAGE NoMonomorphismRestriction #-}

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

-- text, links, bold/italic (things inside paragraphs)
lineContent :: Parser String
lineContent =
  -- TODO: Single punctuation characters (,.?!:;"') at the end of URLs should not be considered part of the URL.
      try nakedLink
  <|> try textInBold
  <|> try textInItalics
  <|> try (do
              word <- many1 (noneOf " \n")
              rest <- lineContent
              return $ word ++ rest)
  <|> try (do
              whitespace <- many1 $ oneOf " \t"
              rest <- lineContent
              return $ whitespace ++ rest)
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
  
  
unorderedListItem :: Parser (Maybe String)
unorderedListItem =
  try (do
          string "*"
          item <- restOfLine
          return $ Just item
      )
  <|> return Nothing

-- todo: leading whitespace
unorderedListItems :: Parser [String]
unorderedListItems = do
  item <- unorderedListItem
  case item of
    Just line -> do
      rest <- unorderedListItems
      return $ line : rest
    Nothing -> return []

unorderedList = do
  items <- unorderedListItems
  newline
  let liTags = map (\s -> "<li>" ++ s ++ "</li>\n") items
  let liTagsJoined = foldr (++) "" liTags
  return $ "<ul>\n" ++ liTagsJoined ++ "</ul>\n"

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
  
out1 = parseTest creole "== title ==\nfoo bar http://example.com\n\n"
out2 = parseTest creole "{{{ foo }}}"
out3 = parseTest creole "some **bold** text and //italics// too!\n\n"
out4 = parseTest creole "*foo\n*bar\n\n"
