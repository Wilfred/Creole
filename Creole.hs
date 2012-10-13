{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple  = letter

-- todo: whitespace on blank line
emptyLine :: Parser String
emptyLine = many1 newline

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
  -- <|> do
  --       emptyLine
  --       return ""
  <|> return ""

-- either a paragraph or a heading
-- TODO: headings may end with matching == too
block :: Parser String
block = 
  try (do
    string "=="
    spaces
    heading <- many $ noneOf "\n"
    return $ "<h2>" ++ heading ++ "</h2>")
  <|> try (do
    char '='
    spaces
    heading <- many $ noneOf "\n"
    return $ "<h1>" ++ heading ++ "</h1>")
  <|> try (do
    paragraph <- lineContent
    return $ "<p>" ++ paragraph ++ "</p>")

paragraphs :: Parser [String]
paragraphs = do
  paras <- (many $ noneOf "\n") `sepBy` emptyLine
  return $ map (\s -> "<p>" ++ s ++ "</p>\n") paras

-- defined in Parsec as parseTest
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do
              putStr "parse error at "
              print err
            Right x -> print x
            
out1 = run simple "z"


