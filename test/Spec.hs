import Data.Text.IO qualified as TI
import Parser
import Pre qualified
import Pre.Internal
import Test.Hspec

main :: IO ()
main = do
  f <- TI.readFile "test/test.tl"
  hspec $ do
    describe "comment split" $ do
      it "" $ do
        commentSplit
          "//@description Returns q @query Query w @message_thread_id If e\nsupergroupMembersFilterMention query:string message_thread_id:int53 = SupergroupMembersFilter;"
          `shouldBe` "//@description Returns q \n//@query Query w \n//@message_thread_id If e\nsupergroupMembersFilterMention query:string message_thread_id:int53 = SupergroupMembersFilter;"
    describe "parser test" $ do
      it "from file" $ do
        Pre.parse f
          `shouldBe` Right
            ( [ Class (ClassName "AuthenticationCodeType") "Provides information about the method by which an authentication code is delivered to the user",
                Class
                  { name = ClassName "EmailAddressAuthentication",
                    comment = "Contains authentication data for a email address"
                  }
              ],
              [ Method
                  { name = "authenticationCodeTypeTelegramMessage",
                    comment = "An authentication code is delivered via a private Telegram message, which can be viewed from another active session",
                    args =
                      [ Arg
                          { name = "length",
                            value = TInt32,
                            comment = Just "Left time before the email",
                            null = False
                          }
                      ],
                    result = ClassName "AuthenticationCodeType"
                  },
                Method
                  { name = "authenticationCodeTypeSms",
                    comment = "An authentication code is delivered via an SMS message to the specified phone number; applications may not receive this type of code",
                    args =
                      [ Arg
                          { name = "reset_in",
                            value = TInt32,
                            comment = Nothing,
                            null = False
                          }
                      ],
                    result = ClassName "AuthenticationCodeType"
                  },
                Method
                  { name = "authenticationCodeTypeCall",
                    comment = "An authentication code is delivered via a phone call to the specified phone number",
                    args =
                      [ Arg
                          { name = "length",
                            value = TInt32,
                            comment = Just "Length of the code",
                            null = False
                          }
                      ],
                    result = ClassName "AuthenticationCodeType"
                  }
              ],
              [ Method
                  { name = "stickerFullTypeRegular",
                    comment = "The sticker is a regular sticker",
                    args =
                      [ Arg
                          { name = "premium_animation",
                            value = TModule "file",
                            comment = Just "Premium animation of the sticker; may be null. If present, only Telegram Premium users can use the sticker",
                            null = True
                          }
                      ],
                    result = ClassName "StickerFullType"
                  },
                Method
                  { name = "closedVectorPath",
                    comment = "Represents a closed vector path.",
                    args =
                      [ Arg
                          { name = "commands",
                            value = TVector (TVector (TModule "VectorPathCommand")),
                            comment = Nothing,
                            null = False
                          }
                      ],
                    result = ClassName "ClosedVectorPath"
                  }
              ]
            )
