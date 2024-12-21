

//@class LogStream @description Describes a stream to which TDLib internal log is written



//@description Contains full information about a user
//@personal_photo User profile photo set by the current user for the contact; may be null. If null and user.profile_photo is null, then the photo is empty; otherwise, it is unknown.
//-If non-null, then it is the same photo as in user.profile_photo and chat.photo. This photo isn't returned in the list of user photos
//@photo User profile photo; may be null. If null and user.profile_photo is null, then the photo is empty; otherwise, it is unknown.
//-If non-null and personal_photo is null, then it is the same photo as in user.profile_photo and chat.photo
//@public_photo User profile photo visible if the main photo is hidden by privacy settings; may be null. If null and user.profile_photo is null, then the photo is empty; otherwise, it is unknown.
//-If non-null and both photo and personal_photo are null, then it is the same photo as in user.profile_photo and chat.photo. This photo isn't returned in the list of user photos
//@block_list Block list to which the user is added; may be null if none
//@can_be_called True, if the user can be called
//@supports_video_calls True, if a video call can be created with the user
//@has_private_calls True, if the user can't be called due to their privacy settings
//@has_private_forwards True, if the user can't be linked in forwarded messages due to their privacy settings
//@has_restricted_voice_and_video_note_messages True, if voice and video notes can't be sent or forwarded to the user
//@has_posted_to_profile_stories True, if the user has posted to profile stories
//@has_sponsored_messages_enabled True, if the user always enabled sponsored messages; known only for the current user
//@need_phone_number_privacy_exception True, if the current user needs to explicitly allow to share their phone number with the user when the method addContact is used
//@set_chat_background True, if the user set chat background for both chat users and it wasn't reverted yet
//@bio A short user bio; may be null for bots
//@birthdate Birthdate of the user; may be null if unknown
//@personal_chat_id Identifier of the personal chat of the user; 0 if none
//@gift_count Number of gifts saved to profile by the user
//@group_in_common_count Number of group chats where both the other user and the current user are a member; 0 for the current user
//@business_info Information about business settings for Telegram Business accounts; may be null if none
//@bot_info For bots, information about the bot; may be null if the user isn't a bot
userFullInfo personal_photo:chatPhoto photo:chatPhoto public_photo:chatPhoto block_list:BlockList can_be_called:Bool supports_video_calls:Bool has_private_calls:Bool has_private_forwards:Bool has_restricted_voice_and_video_note_messages:Bool has_posted_to_profile_stories:Bool has_sponsored_messages_enabled:Bool need_phone_number_privacy_exception:Bool set_chat_background:Bool bio:formattedText birthdate:birthdate personal_chat_id:int53 gift_count:int32 group_in_common_count:int32 business_info:businessInfo bot_info:botInfo = UserFullInfo;



---functions---



//@description Returns the current authorization state; this is an offline request. For informational purposes only. Use updateAuthorizationState instead to maintain the current authorization state. Can be called before initialization
getAuthorizationState = AuthorizationState;


