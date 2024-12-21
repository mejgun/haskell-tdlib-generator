import Data.Text.IO qualified as TI
import Lib ()
import Parser
  ( Arg (..),
    ArgVal (..),
    Class (..),
    ClassName (..),
    Method (..),
    parse,
  )
import Parser.Internal
  ( parseClass,
    parseMethod,
  )
import Pre (prepare)
import Pre.Internal
  ( commentSplit,
  )
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = do
  f <- TI.readFile "test/test.tl"
  f_pre <- TI.readFile "test/test_pre.tl"
  hspec $ do
    describe "comment split" $ do
      it "" $ do
        commentSplit
          "//@description Returns q @query Query w @message_thread_id If e\nsupergroupMembersFilterMention query:string message_thread_id:int53 = SupergroupMembersFilter;"
          `shouldBe` "//@description Returns q \n//@query Query w \n//@message_thread_id If e\nsupergroupMembersFilterMention query:string message_thread_id:int53 = SupergroupMembersFilter;"

    describe "class parse" $ do
      it "" $ do
        parseClass ["//@class CanTransferOwnershipResult @description Represents result of checking whether the current session can be used to transfer a chat ownership to another user"]
          `shouldBe` Right
            Class
              { name = ClassName "CanTransferOwnershipResult",
                comment = "Represents result of checking whether the current session can be used to transfer a chat ownership to another user"
              }

    describe "method parse" $ do
      it "" $ do
        parseMethod
          [ "//@@description A digit-only authentication code is delivered via Firebase Authentication to the official Android application",
            "//@device_verification_parameters Parameters to be used for device verification",
            "//@length Length of the code",
            "authenticationCodeTypeFirebaseAndroid device_verification_parameters:FirebaseDeviceVerificationParameters length:int32 = AuthenticationCodeType;"
          ]
          `shouldBe` Right
            Method
              { name = "authenticationCodeTypeFirebaseAndroid",
                comment = "A digit-only authentication code is delivered via Firebase Authentication to the official Android application",
                args =
                  [ Arg
                      { name = "device_verification_parameters",
                        value = TModule "FirebaseDeviceVerificationParameters",
                        comment = Just "Parameters to be used for device verification"
                      },
                    Arg
                      { name = "length",
                        value = TInt32,
                        comment = Just "Length of the code"
                      }
                  ],
                result = ClassName "AuthenticationCodeType"
              }

    describe "prepare test" $ do
      it "" $
        do
          prepare f_pre
          `shouldBe` ( [ [ "//@class LogStream @description Describes a stream to which TDLib internal log is written"
                         ],
                         [ "//@@description Contains full information about a user",
                           "//@personal_photo User profile photo set by the current user for the contact; may be null. If null and user.profile_photo is null, then the photo is empty; otherwise, it is unknown. If non-null, then it is the same photo as in user.profile_photo and chat.photo. This photo isn't returned in the list of user photos",
                           "//@photo User profile photo; may be null. If null and user.profile_photo is null, then the photo is empty; otherwise, it is unknown. If non-null and personal_photo is null, then it is the same photo as in user.profile_photo and chat.photo",
                           "//@public_photo User profile photo visible if the main photo is hidden by privacy settings; may be null. If null and user.profile_photo is null, then the photo is empty; otherwise, it is unknown. If non-null and both photo and personal_photo are null, then it is the same photo as in user.profile_photo and chat.photo. This photo isn't returned in the list of user photos",
                           "//@block_list Block list to which the user is added; may be null if none",
                           "//@can_be_called True, if the user can be called",
                           "//@supports_video_calls True, if a video call can be created with the user",
                           "//@has_private_calls True, if the user can't be called due to their privacy settings",
                           "//@has_private_forwards True, if the user can't be linked in forwarded messages due to their privacy settings",
                           "//@has_restricted_voice_and_video_note_messages True, if voice and video notes can't be sent or forwarded to the user",
                           "//@has_posted_to_profile_stories True, if the user has posted to profile stories",
                           "//@has_sponsored_messages_enabled True, if the user always enabled sponsored messages; known only for the current user",
                           "//@need_phone_number_privacy_exception True, if the current user needs to explicitly allow to share their phone number with the user when the method addContact is used",
                           "//@set_chat_background True, if the user set chat background for both chat users and it wasn't reverted yet",
                           "//@bio A short user bio; may be null for bots",
                           "//@birthdate Birthdate of the user; may be null if unknown",
                           "//@personal_chat_id Identifier of the personal chat of the user; 0 if none",
                           "//@gift_count Number of gifts saved to profile by the user",
                           "//@group_in_common_count Number of group chats where both the other user and the current user are a member; 0 for the current user",
                           "//@business_info Information about business settings for Telegram Business accounts; may be null if none",
                           "//@bot_info For bots, information about the bot; may be null if the user isn't a bot",
                           "userFullInfo personal_photo:chatPhoto photo:chatPhoto public_photo:chatPhoto block_list:BlockList can_be_called:Bool supports_video_calls:Bool has_private_calls:Bool has_private_forwards:Bool has_restricted_voice_and_video_note_messages:Bool has_posted_to_profile_stories:Bool has_sponsored_messages_enabled:Bool need_phone_number_privacy_exception:Bool set_chat_background:Bool bio:formattedText birthdate:birthdate personal_chat_id:int53 gift_count:int32 group_in_common_count:int32 business_info:businessInfo bot_info:botInfo = UserFullInfo;"
                         ]
                       ],
                       [ [ "//@@description Returns the current authorization state; this is an offline request. For informational purposes only. Use updateAuthorizationState instead to maintain the current authorization state. Can be called before initialization",
                           "getAuthorizationState = AuthorizationState;"
                         ]
                       ]
                     )

    describe "parser test" $ do
      it "from file" $ do
        (parse . prepare) f
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
                            comment = Just "Left time before the email"
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
                            comment = Nothing
                          }
                      ],
                    result = ClassName "AuthenticationCodeType"
                  },
                Method
                  { name = "authenticationCodeTypeCall",
                    comment = "An authentication code is delivered via a phone call to the specified phone number",
                    args =
                      [ Arg
                          { name = "description",
                            value = TInt32,
                            comment = Just "Length of the code"
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
                            comment = Just "Premium animation of the sticker; may be null. If present, only Telegram Premium users can use the sticker"
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
                            comment = Nothing
                          }
                      ],
                    result = ClassName "ClosedVectorPath"
                  }
              ]
            )
