package monkeylang.web

import androidx.compose.runtime.Composable
import monkeylang.Lexer
import monkeylang.Token
import monkeylang.TokenType
import org.jetbrains.compose.web.dom.Div
import org.jetbrains.compose.web.dom.Span
import org.jetbrains.compose.web.dom.Text

@Composable
fun DebugTokenView(code: String) {
    val lexer = Lexer(code)

    Div({
        classes(DebugStyleSheet.debugTokenContainer)
    }) {
        while (lexer.hasNext()) {
            val token = lexer.next()
            DebugToken(token)
        }
    }
}

@Composable
fun DebugToken(token: Token) {
    Div({
        classes(DebugStyleSheet.debugToken)
        title(token.type.name)
    }) {
        when (token.type) {
            TokenType.EndOfFile -> {
                Text("EOF")
            }
            else -> {
                Text(token.literal)
                Span({
                    classes(DebugStyleSheet.debugTokenRange)
                }) {
                    Text(" ${token.type.name} [${token.range}]")
                }
            }
        }
    }
}
