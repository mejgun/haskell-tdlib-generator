//@class AuthenticationCodeType @description Provides information about the method by which an authentication code is delivered to the user
//@description An authentication code is delivered via a private Telegram message, which can be viewed from another active session
//@length Left time before the email
authenticationCodeTypeTelegramMessage length:int32 = AuthenticationCodeType;
//@description An authentication code is delivered via an SMS message to the specified phone number; applications may not receive this type of code
authenticationCodeTypeSms reset_in:int32 = AuthenticationCodeType;
//@class EmailAddressAuthentication @description Contains authentication data for a email address
//@description An authentication code is delivered via a phone call to the specified phone number
//@length Length of the code
authenticationCodeTypeCall length:int32 = AuthenticationCodeType;
//@description The sticker is a regular sticker
//@premium_animation Premium animation of the sticker; may be null. If present, only Telegram Premium users can use the sticker
stickerFullTypeRegular premium_animation:file = StickerFullType;
//@description Represents a closed vector path.
closedVectorPath commands:vector<VectorPathCommand> = ClosedVectorPath;
