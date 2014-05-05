module Test where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)
import Text.Parsec.Error
import Text.Parsec.Pos

import Test.HUnit
import Parser

errorMessage :: Message -> String
errorMessage (Expect s) = "Expected input: " ++ s
errorMessage (Message s) = "Parse error: " ++ s
errorMessage _ = ""

testParser :: FilePath -> Test
testParser file = TestCase $ do
	input <- readFile file
	case testP namespace input of
		Right _ -> assert ()
		Left pe ->
			let sp = errorPos pe in
			let l = sourceLine sp in
			let c = sourceColumn sp in
			let messages = errorMessages pe in
			assertFailure $ "Parse error in file '" ++ file ++ "' at line " ++ show l ++ " col " ++ show c
				++ ":\n" ++ (concatMap (\s -> "\t" ++ s ++ "\n") . filter (not . null) . map errorMessage) messages

getDrakeFiles :: FilePath -> IO [FilePath]
getDrakeFiles dir = map (dir ++) <$> filter (isSuffixOf ".drake") <$> getDirectoryContents dir 

runTestCases :: IO [Counts]
runTestCases = do
	parserTests <- getDrakeFiles "tests/parser/"
	let parserTestCases = map testParser parserTests
	forM parserTestCases runTestTT