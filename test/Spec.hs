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
      runCssParser "h1 { font-size: 48px; }\nh2 { font-size: 24px; }"
        `shouldBe` Just [Form "h1" (RuleSet [Rule "font-size" "48px"]),
                         Form "h2" (RuleSet [Rule "font-size" "24px"])]
