import Parser qualified
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "classParser" $ do
    it "single line class comment" $ do
      parse Parser.classParser "test" "//@class AuthenticationCodeType @description Provides information about the method by which an authentication code is delivered to the user\n"
        `shouldBe` Right (Parser.Class "AuthenticationCodeType" "Provides information about the method by which an authentication code is delivered to the user")

  describe "allParser" $ do
    it "class comments" $ do
      parse
        Parser.allParser
        "test"
        "//@class BackgroundFill @description Describes a fill of a background\n\
        \//@class BackgroundType @description Describes the type of a background\n\
        \//@class DeviceToken @description Represents a data needed to subscribe for push notifications through registerDevice method. To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org\n\
        \//@class InputBackground @description Contains information about background to set\n\n"
        `shouldBe` Right
          [ Parser.Class "BackgroundFill" "Describes a fill of a background",
            Parser.Class "BackgroundType" "Describes the type of a background",
            Parser.Class "DeviceToken" "Represents a data needed to subscribe for push notifications through registerDevice method. To use specific push notification service, the correct application platform must be specified and a valid server authentication data must be uploaded at https://my.telegram.org",
            Parser.Class "InputBackground" "Contains information about background to set"
          ]

  describe "methodParser" $ do
    it "" $ do
      parse
        Parser.methodParser
        "test"
        "//@description Describes an image in JPEG format\n\
        \//@type Image type (see https://core.telegram.org/constructor/photoSize)\n\
        \//@photo Information about the image file\n\
        \//@width Image width\n\
        \//@height Image height\n\
        \//@progressive_sizes Sizes of progressive JPEG file prefixes, which can be used to preliminarily show the image; in bytes\n\
        \photoSize type:string photo:file width:int32 height:int32 progressive_sizes:vector<int32> = PhotoSize;\n"
        `shouldBe` Right (Parser.Method "photoSize" "Describes an image in JPEG format" [] "PhotoSize")