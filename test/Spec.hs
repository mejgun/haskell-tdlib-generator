import Parser qualified
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "classParser" $ do
    it "single line class comment" $ do
      parse Parser.classParser "test" "//@class AuthenticationCodeType @description Provides information about the method by which an authentication code is delivered to the user"
        `shouldBe` ( Right $
                       Parser.Class "AuthenticationCodeType" "Provides information about the method by which an authentication code is delivered to the user"
                   )

    it "multi line class comment" $ do
      parse Parser.classParser "test" "//@class DeviceToken @description Represents a data needed to subscribe for push notifications through registerDevice method.\n//-To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org"
        `shouldBe` ( Right $
                       Parser.Class "DeviceToken" "Represents a data needed to subscribe for push notifications through registerDevice method. To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org"
                   )
