--
-- HSpec tests for ParseSymbols.hs
--
-- Created by Nick Ager on 25/03/2015.
--
-- see HSpec documentation - http://hspec.github.io
--
--
-- to run the tests execute:
--
-- $ stack test
--
-- the above will install any dependencies and run the tests ie this file.

{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ParseSymbols hiding (main)
import Text.Parsec.String.Parsec (parse)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (ParseError)
import Data.Functor

objCInstanceMethodTestText    = "                0x0000bfe4 (    0x78) -[AnAppDelegate initialViewController] [FUNC, OBJC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
cMethodTestText               = "                0x00008140 (   0x1a4) setupVPCustomClass [FUNC, THUMB, EXT, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
cMethod2TestText              = "                0x00874d90 (    0x88) internal_callback_iterator(int, __siginfo*, __darwin_ucontext*, void*) [FUNC, THUMB, NameNList, MangledNameNList, NList]\n"
objCMethodWithOffsetTestText  = "                0x00008994 (    0x6a) __60+[ClosingTheLoopMessageStrings stringFromEscalationActions:]_block_invoke [FUNC, THUMB, LENGTH, NameNList, MangledNameNList, Merged, NList, Dwarf]\n"
addressLineTestText           = "                    0x0000827c (    0x28) main.m:21\n"
cMethod3TestText              = "                0x00347764 (    0x6c) plcrash_error_t pl_async_objc_parse_objc2_class_methods<pl_objc2_class_32, pl_objc2_class_data_ro_32, pl_objc2_class_data_rw_32>(plcrash_async_macho*, plcrash_async_objc_cache*, pl_objc2_class_32*, bool, void (*)(bool, plcrash_async_macho_string*, plcrash_async_macho_string*, unsigned int, void*), void*) [FUNC, THUMB, NameNList, MangledNameNList, NList]\n"
cMethodWithNumberTestText     = "                0x003478f4 (    0xdc) pl_async_objc_parse_objc2_method_list(plcrash_async_macho*, plcrash_async_objc_cache*, plcrash_async_macho_string*, bool, unsigned int, void (*)(bool, plcrash_async_macho_string*, plcrash_async_macho_string*, unsigned int, void*), void*) [FUNC, THUMB, NameNList, MangledNameNList, NList]\n"

parseResult :: Parser a -> String -> a
parseResult p target = convertParseResult $ parse p "" target

parseFileForTest :: Parser a -> String -> IO a
parseFileForTest parser file = do
    parsedResult <- parseFromFile parser file
    return $ convertParseResult parsedResult

--

convertParseResult :: Either ParseError a -> a
convertParseResult (Left errorResult) = error $ show  errorResult
convertParseResult (Right result) = result

--

main :: IO ()
main = hspec $ do
    describe "methodNameObjC" $ do
        it "parse ObjC instance method" $
            parseResult methodNameObjC "-[AnAppDelegate initialViewController]" `shouldBe` "-[AnAppDelegate initialViewController]"
        it "parse ObjC instance method with Prefix" $
            parseResult methodNameObjCWithPrefix "__60+[ClosingTheLoopMessageStrings stringFromEscalationActions:]" `shouldBe` "+[ClosingTheLoopMessageStrings stringFromEscalationActions:]"

    describe "methodNameLine" $ do
        it "parse ObjC instance method" $
            parseResult methodNameLine objCInstanceMethodTestText `shouldBe` "-[AnAppDelegate initialViewController]"
        it "parse ObjC method with offset" $
            parseResult methodNameLine objCMethodWithOffsetTestText `shouldBe` "+[ClosingTheLoopMessageStrings stringFromEscalationActions:]"
        it "parse C method" $
            parseResult methodNameLine cMethodTestText `shouldBe` "setupVPCustomClass"
        it "parse C method2" $
            parseResult methodNameLine cMethod2TestText `shouldBe` "internal_callback_iterator"
        it "parse C method3" $
            parseResult methodNameLine cMethod3TestText `shouldBe` "plcrash_error_t"
        it "parse C method with number" $
            parseResult methodNameLine cMethodWithNumberTestText `shouldBe` "pl_async_objc_parse_objc2_method_list"


    describe "addressLine" $ do
        it "parses filename" $
            file (parseResult (addressLine "-[AClass aMethod]") addressLineTestText) `shouldBe` "main.m"
        it "parses line number" $
            line (parseResult (addressLine "-[AClass aMethod]") addressLineTestText) `shouldBe` 21
        it "parses start address" $
            startAddress (parseResult (addressLine "-[AClass aMethod]") addressLineTestText) `shouldBe` "0x0000827c"
        it "parses end address" $
            endAddress (parseResult (addressLine "-[AClass aMethod]") addressLineTestText) `shouldBe` "0x000082a4"
        it "copies method name" $
            methodName (parseResult (addressLine "-[AClass aMethod]") addressLineTestText) `shouldBe` "-[AClass aMethod]"

    describe "ParseSymbolsInput" $
        it "parses file with the correct number of methods" $
            (length <$> parseFileForTest parseSymbolsOutput "ParseSymbolsInput.txt") `shouldReturn` 33

    describe "ParseSymbolsInput 2" $ do
        it "parses file with the correct details for the first method" $ do
            symbols <- parseFileForTest parseSymbolsOutput "ParseSymbolsInput.txt"
            let firstSymbol = head symbols
            return (methodName firstSymbol) `shouldReturn` "setupVPCustomClass"
            return (startAddress firstSymbol) `shouldReturn` "0x00008140"
            return (endAddress firstSymbol) `shouldReturn` "0x0000816a"
        it "parses file with the correct details for the third method" $ do
            symbols <- parseFileForTest parseSymbolsOutput "ParseSymbolsInput.txt"
            let thirdMethodFirstSymbols = symbols !! 15
            return (methodName thirdMethodFirstSymbols) `shouldReturn` "+[ClosingTheLoopMessageStrings ddLogLevel]"
            return (startAddress thirdMethodFirstSymbols) `shouldReturn` "0x00008374"
            return (endAddress thirdMethodFirstSymbols) `shouldReturn` "0x00008384"
