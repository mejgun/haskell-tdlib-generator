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
                          realname = "length",
                          value = TInt32,
                          comment = Just "Left time before the email",
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
                        { name = "reset_in",
                          realname = "reset_in",
                          value = TInt32,
                          comment = Nothing,
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
                          realname = "length",
                          value = TInt32,
                          comment = Just "Length of the code",
                          null = False
                        }
                    ],
                  result = "AuthenticationCodeType"
                },
              Method
                { name = "stickerFullTypeRegular",
                  comment = "The sticker is a regular sticker",
                  args =
                    [ Arg
                        { name = "premium_animation",
                          realname = "premium_animation",
                          value = TModule "file",
                          comment = Just "Premium animation of the sticker; may be null. If present, only Telegram Premium users can use the sticker",
                          null = True
                        }
                    ],
                  result = "StickerFullType"
                },
              Method
                { name = "closedVectorPath",
                  comment = "Represents a closed vector path.",
                  args =
                    [ Arg
                        { name = "commands",
                          realname = "commands",
                          value = TVector (TModule "VectorPathCommand"),
                          comment = Nothing,
                          null = False
                        }
                    ],
                  result = "ClosedVectorPath"
                }
            ]
