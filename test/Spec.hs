import Test.Hspec
import Text.CSS.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing CSS forms" $ do
    it "should parse basic forms" $ do
      runCssParser "body { background: #333; }"
        `shouldBe` Just [Form "body" (RuleSet [Rule "background" "#333"])]
