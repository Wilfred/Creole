import Test.HUnit
import Text.ParserCombinators.Parsec
import Creole

parserOutput :: Parser String -> String -> String
parserOutput parser text =
  case parse parser "" text of
    Left _ -> ""
    Right x -> x
    
assertRendersTo :: String -> String -> String -> Test
assertRendersTo message source output =
  TestCase (assertEqual message output (parserOutput creole source))

-- todo: factor out assertion
testTitle = 
  assertRendersTo "Renders a title"
  "== title ==\n"
  "<h2>title </h2>\n"

testNamedLink = 
  assertRendersTo
  "Renders a named link"
  "[[foo|bar]]\n\n"
  "<p><a href=\"foo\">bar</a></p>\n"
  
testBullets = 
  assertRendersTo
  "Renders two levels of unordered list"
  "* foo\n** bar\n"
  "<ul>\n<li> foo</li><li><ul><li> bar</li></ul></li></ul>\n"
  
testBoldText =
  assertRendersTo "Renders bold text"
  "some **bold** text\n\n"
  "<p>some <strong>bold</strong> text</p>\n"

tests = TestList [testTitle, testNamedLink, testBullets, testBoldText]

main = runTestTT tests