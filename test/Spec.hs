import Test.Hspec
import Text.CSS.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing CSS forms" $ do
    it "should parse basic rule" $ do
      1 `shouldBe` 2
