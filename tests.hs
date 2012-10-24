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
  
testNakedLink =
  assertRendersTo
  "Renders a naked link"
  "http://example.com\n\n"
  "<p><a href=\"http://example.com\">http://example.com</a></p>\n"
  

testNamedLink = 
  assertRendersTo
  "Renders a named link"
  "[[foo|bar]]\n\n"
  "<p><a href=\"foo\">bar</a></p>\n"
  
testFlatBullets =
  assertRendersTo
  "Renders flat unordered list"
  "* foo\n* bar\n\n"
  "<ul><li>foo</li><li>bar</li></ul>\n"
  
testNestedBullets = 
  assertRendersTo
  "Renders a nested unordered list"
  "* foo\n** bar\n\n"
  "<ul><li>foo<ul><li>bar</li></ul></li></ul>\n"
  
testBoldText =
  assertRendersTo "Renders bold text"
  "some **bold** text\n\n"
  "<p>some <strong>bold</strong> text</p>\n"

testItalicText =
  assertRendersTo "Renders italic text"
  "some //italic// text\n\n"
  "<p>some <em>italic</em> text</p>\n"

tests = TestList [testTitle, testNamedLink, testNakedLink, testFlatBullets, 
                  testNestedBullets, testBoldText, testItalicText]

main = runTestTT tests