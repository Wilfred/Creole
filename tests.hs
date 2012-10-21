import Test.HUnit
import Text.ParserCombinators.Parsec
import Creole

parserOutput :: Parser String -> String -> String
parserOutput parser text =
  case parse parser "" text of
    Left _ -> ""
    Right x -> x

testTitle = 
  TestCase (assertEqual 
            "Renders a title"
            (parserOutput creole "== title ==\n")
            "<h2>title </h2>\n")

testNamedLink = 
  TestCase (assertEqual 
            "Renders a named link"
            (parserOutput creole "[[foo|bar]]\n\n")
            "<p><a href=\"foo\">bar</a></p>\n")

tests = TestList [testTitle, testNamedLink]

main = runTestTT tests