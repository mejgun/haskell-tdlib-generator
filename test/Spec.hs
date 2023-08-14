import Parser qualified
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "classParser" $ do
    it "single line class comment" $ do
      parse Parser.classParser "test" "//@class AuthenticationCodeType @description Provides information about the method by which an authentication code is delivered to the user\n\n"
        `shouldBe` ( Right $
                       Parser.Class "AuthenticationCodeType" "Provides information about the method by which an authentication code is delivered to the user"
                   )

    it "multi line class comment" $ do
      parse
        Parser.classParser
        "test"
        "//@class DeviceToken @description Represents a data needed to subscribe for push notifications through registerDevice method.\n\
        \//-To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org"
        `shouldBe` ( Right $
                       Parser.Class "DeviceToken" "Represents a data needed to subscribe for push notifications through registerDevice method. To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org"
                   )

  describe "allParser" $ do
    it "class comments" $ do
      parse
        Parser.allParser
        "test"
        "//@class BackgroundFill @description Describes a fill of a background\n\
        \\n\n\
        \//@class BackgroundType @description Describes the type of a background\n\
        \\
        \//@class DeviceToken @description Represents a data needed to subscribe for push notifications through registerDevice method.\n\
        \//-To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org\n\
        \\n\n\n\
        \//@class InputBackground @description Contains information about background to set"
        `shouldBe` ( Right
                       [ Parser.Class "BackgroundFill" "Describes a fill of a background",
                         Parser.Class "BackgroundType" "Describes the type of a background",
                         Parser.Class "DeviceToken" "Represents a data needed to subscribe for push notifications through registerDevice method. To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org",
                         Parser.Class "InputBackground" "Contains information about background to set"
                       ]
                   )
