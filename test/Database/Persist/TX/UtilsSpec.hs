module Database.Persist.TX.UtilsSpec where

-- {{{1 imports
import           ClassyPrelude
import           Test.Hspec

import Database.Persist.TX.Utils
-- }}}1


spec :: Spec
spec = do
  describe "autoNameNumAscending" $ do
    it "rename english file path with ext part" $ do
      autoNameNumAscending "abc.txt" `shouldBe` "abc(1).txt"
      autoNameNumAscending "abc(1).txt" `shouldBe` "abc(2).txt"
      autoNameNumAscending "abc(2).txt" `shouldBe` "abc(3).txt"

    it "rename english file path without ext part" $ do
      autoNameNumAscending "abc" `shouldBe` "abc(1)"
      autoNameNumAscending "abc(1)" `shouldBe` "abc(2)"
      autoNameNumAscending "abc(2)" `shouldBe` "abc(3)"

    it "rename Chinese file path with ext part" $ do
      autoNameNumAscending "又长又无聊.txt" `shouldBe` "又长又无聊(1).txt"
      autoNameNumAscending "又长又无聊(1).txt" `shouldBe` "又长又无聊(2).txt"
      autoNameNumAscending "又长又无聊(2).txt" `shouldBe` "又长又无聊(3).txt"

    it "rename english file path without ext part" $ do
      autoNameNumAscending "又长又无聊" `shouldBe` "又长又无聊(1)"
      autoNameNumAscending "又长又无聊(1)" `shouldBe` "又长又无聊(2)"
      autoNameNumAscending "又长又无聊(2)" `shouldBe` "又长又无聊(3)"

-- vim: set foldmethod=marker:
