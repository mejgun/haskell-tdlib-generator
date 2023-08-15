import Parser
import Test.Hspec
import Text.Megaparsec qualified as M

main :: IO ()
main = do
  f <- readFile "test/test.tl"
  hspec $ do
    describe "parser test" $ do
      it "from file" $ do
        M.parse allParser "test file" f
          `shouldBe` Right
            [ Class "AuthenticationCodeType" "Provides information about the method by which an authentication code is delivered to the user",
              Method
                { name = "authenticationCodeTypeTelegramMessage",
                  comment = "An authentication code is delivered via a private Telegram message, which can be viewed from another active session",
                  args =
                    [ Arg
                        { name = "length",
                          value = TInt32,
                          comment = "Length of the code",
                          null = False
                        }
                    ],
                  result = "AuthenticationCodeType"
                },
              Method
                { name = "authenticationCodeTypeSms",
                  comment = "An authentication code is delivered via an SMS message to the specified phone number; applications may not receive this type of code",
                  args =
                    [ Arg
                        { name = "length",
                          value = TInt32,
                          comment = "Length of the code",
                          null = False
                        }
                    ],
                  result = "AuthenticationCodeType"
                },
              Class
                { name = "EmailAddressAuthentication",
                  comment = "Contains authentication data for a email address"
                },
              Method
                { name = "authenticationCodeTypeCall",
                  comment = "An authentication code is delivered via a phone call to the specified phone number",
                  args =
                    [ Arg
                        { name = "length",
                          value = TInt32,
                          comment = "Length of the code",
                          null = False
                        }
                    ],
                  result = "AuthenticationCodeType"
                }
            ]
