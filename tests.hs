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

tests = TestList [testTitle]

main = runTestTT tests