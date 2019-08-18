import BinUtilsTest
import Test.HUnit


thisTest :: Test
thisTest = TestList [binUtilsTest]

main :: IO ()
main = do
  runTestTT thisTest
  return ()
