package monkeylang.web

import androidx.compose.runtime.* // ktlint-disable no-wildcard-imports
import org.jetbrains.compose.web.dom.* // ktlint-disable no-wildcard-imports

@Composable
fun DebugConsole() {
    val initialContent = """
        let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
          x + y;
        };
        
        let result = add(five, ten);
    """.trimIndent()

    var content by remember { mutableStateOf(initialContent) }

    Div({
        classes(DebugStyleSheet.layout)
    }) {
        Div({
            classes(DebugStyleSheet.panel)
        }) {
            DebugTextEditor(
                initialContent = initialContent,
                onContentChange = { newContent -> content = newContent },
            )
        }

        Div({
            classes(DebugStyleSheet.panel)
        }) {
            DebugTokenView(content)
        }
    }
}
