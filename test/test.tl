//@class AuthenticationCodeType @description Provides information about the method by which an authentication code is delivered to the user
//@description An authentication code is delivered via a private Telegram message, which can be viewed from another active session
//@length Length of the code
authenticationCodeTypeTelegramMessage length:int32 = AuthenticationCodeType;
//@description An authentication code is delivered via an SMS message to the specified phone number; applications may not receive this type of code
//@length Length of the code
authenticationCodeTypeSms length:int32 = AuthenticationCodeType;
//@description An authentication code is delivered via a phone call to the specified phone number
//@length Length of the code
authenticationCodeTypeCall length:int32 = AuthenticationCodeType;