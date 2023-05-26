/// monkeydart, Thorsten Ball monkey interpreter written in Dart
///
/// ```sh
/// # activate monkeydart
/// dart pub global activate monkeydart
///
/// # see usage
/// mkay --help
/// ```
library monkeydart;

export './src/interpreter/lexer.dart' show Lexer;
export './src/interpreter/token.dart' show Token, TokenType;
