{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.ParserCombinators.Parsec

-- text, links, bold/italic (things inside paragraphs)
lineContent :: Parser String
lineContent =
  -- TODO: Single punctuation characters (,.?!:;"') at the end of URLs should not be considered part of the URL.
      try (do
              string "http://"
              target <- many $ noneOf " \n"
              rest <- lineContent
              return $ "<a href=\"http://" ++ target ++ "\">" ++ target ++ "</a>" ++ rest)
  <|> try (do
              word <- many1 (noneOf " \n")
              rest <- lineContent
              return $ word ++ rest)
  <|> try (do
              whitespace <- many1 $ oneOf " \t"
              rest <- lineContent
              return $ whitespace ++ rest)
  <|> return ""
      
heading1 = do
  string "="
  spaces
  heading <- many $ noneOf "\n"
  newline
  return $ "<h1>" ++ heading ++ "</h1>"
heading2 = do
  string "=="
  spaces
  heading <- many $ noneOf "\n"
  newline
  return $ "<h2>" ++ heading ++ "</h2>"
  
paragraph = do
  paragraph <- lineContent
  newline
  newline
  return $ "<p>" ++ paragraph ++ "</p>"
  
nowiki = do
  string "{{{"
  nowiki <- many (try (string "}" >> noneOf "}") <|> try (string "}}" >> noneOf "}") <|> noneOf "}")
  string "}}}"
  return $ "<pre>" ++ nowiki ++ "</pre>"
  

-- either a paragraph or a heading
-- TODO: headings may end with matching == too
-- everything ends with a newline, except paragraphs, which require two
block :: Parser String
block = 
  -- need to match subsubheadings first
      try heading2
  <|> try heading1
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

out1 = parseTest creole "== title \nfoo bar http://example.com\n\n"
out2 = parseTest creole "{{{ foo }}}"