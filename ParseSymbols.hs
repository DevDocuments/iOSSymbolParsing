--
-- Transform the output of the Xcode 'symbols' command into JSON:
--
--     $ symbols MyApp.app.dSYM/Contents/Resources/DWARF/MyApp > symbols.txt
--
-- output JSON in form:
-- [
--   {
--   startAddress: "0x00001000", 
--   endAddress: "0x0000100e",
--   messageName: "-[AClass aMethod:]",
--   file: "main.m",
--   line: 10,
--   },
--   ....
-- ]
--
-- build with:
--
-- $ stack build
--
-- (see README.md for more information)
--
-- Created by Nick Ager on 21/3/2015

-- For Aeson support
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseSymbols
( CodeSymbolInfo(methodName, file, line, startAddress, endAddress),
  methodNameLine,
  addressLine,
  methodNameObjC,
  methodNameObjCWithPrefix,
  parseSymbolsOutput,
  main
) where



import Text.Parsec (ParseError)
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, satisfy, hexDigit, string, letter, newline, alphaNum)
import Text.Parsec.String.Combinator (many1, choice, optional, eof, notFollowedBy)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import Text.Printf
import System.Environment
import FunctionsAndTypesForParsing
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BSL

type HexAddress = String -- eg "0x00008140"
type Filename = String
type LineNumber = Integer
type MethodName = String

data CodeSymbolInfo = CodeSymbolInfo {
                      methodName :: MethodName,   -- eg +[ClosingTheLoopMessageStrings ddLogLevel]
                      file :: Filename,           -- eg "main.m"
                      line :: LineNumber,         -- eg 14
                      startAddress :: HexAddress,  -- eg "0x0000827c"
                      endAddress :: HexAddress    -- eg "0x0000845a"
                  } deriving (Eq, Show, Generic)

-- JSON encoding using Generics
instance ToJSON CodeSymbolInfo

parseSymbolsOutput :: Parser [CodeSymbolInfo]
parseSymbolsOutput = do
  parsePrelude
  methodAddresses <- many1 parseMethod
  parseClosing
  eof

  return $ concat methodAddresses


-- "                0x00008994 (    0x6a) __60+[ClosingTheLoopMessageStrings stringFromEscalationActions:]_block_invoke [FUNC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]
-- "                    0x00008994 (    0x38) ClosingTheLoopMessageStrings.m:80
-- "                    0x000089cc (     0x8) ClosingTheLoopMessageStrings.m:80
-- "                    0x000089d4 (    0x1e) ClosingTheLoopMessageStrings.m:81
-- "                    0x000089f2 (     0xc) ClosingTheLoopMessageStrings.m:82
parseMethod :: Parser [CodeSymbolInfo]
parseMethod = do
    methodName <- methodNameLine
    newline
    many $ addressLine methodName

-- examples:
-- "                0x0000bfe4 (    0x78) -[VitalPACAppDelegate initialViewController] [FUNC, OBJC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
-- "                0x00008140 (   0x1a4) setupVPCustomClass [FUNC, THUMB, EXT, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
-- "                0x00008994 (    0x6a) __60+[ClosingTheLoopMessageStrings stringFromEscalationActions:]_block_invoke [FUNC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
methodNameLine :: Parser MethodName
methodNameLine = do
  try methodLineSpacing
  hexAddressCformat
  lengthHex
  choice [internalHelperName, methodNameObjCWithPrefix, methodNameObjC, try methodNameC, try methodNameC2, methodNameC3]

-- example:
-- "                    0x0000827c (    0x28) main.m:21\n"
addressLine :: MethodName -> Parser CodeSymbolInfo
addressLine m =  do
    try addressLineSpacing
    hexAddress <- hexAddressCformat
    alength <- lengthHex
    filename <- filename
    char ':'
    lineNumber <- many1 digit
    consumeCharactersUptoNewLineInclusive
    return CodeSymbolInfo {methodName = m, file = filename, line =  read lineNumber, startAddress = hexAddress, endAddress = endHexFromLengthAddress alength $ read hexAddress}
      where
        endHexFromLengthAddress :: Integer -> Integer -> HexAddress
        endHexFromLengthAddress length2 start = printf "0x%08x" $ length2+ start

-- filename examples:
--   "LocalDatabase+Patient.m"
--   "Location+AutogeneratedProperties.autogen.h"
filename :: Parser Filename
filename = many1 $ satisfy (\a -> isDigit a || isLetter a || a == '_' || a == '+' || a == '.')

-- extract the method name from:
-- -[VitalPACAppDelegate initialViewController] [FUNC, OBJC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]
methodNameObjC :: Parser MethodName
methodNameObjC = do
  methodInstanceType <- oneOf "+-"
  char '['
  methodName <- many $ satisfy (/= ']')
  consumeCharactersUptoNewLine
  return (methodInstanceType : '[' : (methodName ++ "]"))

-- extract the method name from:
-- __60+[ClosingTheLoopMessageStrings stringFromEscalationActions:]_block_invoke [FUNC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
methodNameObjCWithPrefix :: Parser MethodName
methodNameObjCWithPrefix = do
  try methodPrefix
  methodNameObjC
  where
    -- example: __60
    methodPrefix :: Parser ()
    methodPrefix = do
      string "__"
      void $ many1 digit

-- extract the method name from:
-- setupVPCustomClass [FUNC, THUMB, EXT, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]
methodNameC :: Parser MethodName
methodNameC = do
  methodName <- many $ satisfy (/= ' ')
  whitespace
  char '['
  consumeCharactersUptoNewLine
  return methodName

-- extract the method name from:
-- internal_callback_iterator(int, __siginfo*, __darwin_ucontext*, void*) [FUNC, THUMB, NameNList, MangledNameNList, NList]
methodNameC2 :: Parser MethodName
methodNameC2 = do
  firstcharacter <- satisfy (\a -> isLetter a || a == '_')
  methodName <- many $ satisfy (\a -> isLetter a || a == '_' || isDigit a)
  char '('
  consumeCharactersUptoNewLine
  return (firstcharacter : methodName)


-- extract the method name from:
-- plcrash_error_t pl_async_objc_parse_objc2_class_methods<pl_objc2_class_32, pl_objc2_class_data_ro_32, pl_objc2_class_data_rw_32>(plcrash_async_macho*, plcrash_async_objc_cache*, pl_objc2_class_32*, bool, void (*)(bool, plcrash_async_macho_string*, plcrash_async_macho_string*, unsigned int, void*), void*) [FUNC, THUMB, NameNList, MangledNameNList, NList]\n"
methodNameC3 :: Parser MethodName
methodNameC3 = do
  firstcharacter <- satisfy (\a -> isLetter a || a == '_')
  methodName <- many $ satisfy (\a -> isLetter a || a == '_' || isDigit a)
  char ' '
  satisfy (\a -> isLetter a || a == '_')
  consumeCharactersUptoNewLine
  return (firstcharacter : methodName)

-- 0x00008a00 (    0x2a) __copy_helper_block_ [FUNC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]
internalHelperName :: Parser MethodName
internalHelperName = do
  prefix <- try helperPrefix
  methodName <- many $ satisfy (/= ' ')
  consumeCharactersUptoNewLine
  return $ prefix : methodName
    where
        helperPrefix :: Parser Char
        helperPrefix = do
          prefix <- oneOf "_$"
          notFollowedBy $ oneOf " _["
          return prefix

-- example: " (     0xe) "
lengthHex :: Parser Integer
lengthHex = do
    whitespace
    char '('
    whitespace
    length <- hexAddressCformat
    char ')'
    whitespace
    return (read length)

hexAddressCformat :: Parser HexAddress
hexAddressCformat = do
    string "0x"
    address <- many1 hexDigit
    return ("0x" ++ address)

consumeCharactersUptoNewLine :: Parser ()
consumeCharactersUptoNewLine = void $ many $ satisfy (/= '\n')

consumeCharactersUptoNewLineInclusive :: Parser ()
consumeCharactersUptoNewLineInclusive = do
  consumeCharactersUptoNewLine
  void newline

--

methodLineSpacing :: Parser String
methodLineSpacing =  space 16 <?> "method line spacing"

addressLineSpacing :: Parser String
addressLineSpacing = space 20 <?> "address line spacing"

space :: Int -> Parser String
space n = string $ replicate n ' '

consumeLineBeginningWithSpace :: Int -> Parser ()
consumeLineBeginningWithSpace n = do
  try $ space n
  try (notFollowedBy $ char ' ') <?> ("a line beginnning with line spacing: " ++ show n ++ " but the line contained at least one extra space")
  consumeCharactersUptoNewLineInclusive

--

whitespace :: Parser ()
whitespace = void $ many $ char ' '

--

parsePrelude :: Parser ()
parsePrelude = do
  consumeLineBeginningWithSpace 0
  consumeLineBeginningWithSpace 4
  consumeLineBeginningWithSpace 8
  consumeLineBeginningWithSpace 8
  consumeLineBeginningWithSpace 12
  consumeLineBeginningWithSpace 12

parseClosing :: Parser ()
parseClosing = void $ many $ consumeClosingLine
  where consumeClosingLine = consumeLineBeginningWithSpace 16 <|> consumeLineBeginningWithSpace 12 <|> consumeLineBeginningWithSpace 8

intoJSON :: Either ParseError [CodeSymbolInfo] -> BSL.ByteString
intoJSON (Left a) = BSL.pack $ show a
intoJSON (Right b) = encode b

parseFile :: String -> IO ()
parseFile filepath = do
  parsingResults <- parseFromFile parseSymbolsOutput filepath
  putStrLn $ BSL.unpack $ intoJSON parsingResults

-- take a file and parse it
main :: IO ()
main = do
    a <- getArgs
    case a of
      [filepath] -> parseFile filepath
      _ -> error "please pass one argument with the file containing the text to parse"
