package monkeylang.web

import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import org.jetbrains.compose.web.dom.ElementScope
import org.w3c.dom.HTMLDivElement

@Composable
fun ElementScope<HTMLDivElement>.DebugTextEditor(
    initialContent: String,
    onContentChange: (String) -> Unit,
) {
    DisposableEffect(initialContent) {
        val editor = Monaco.editor.create(
            element = scopeElement,
            options = MonacoEditorOptions(
                value = initialContent,
                theme = "vs-dark",
                automaticLayout = true,
            ),
        )

        editor.onDidChangeModelContent {
            val content = editor.getModel().getLinesContent().joinToString("\n")
            onContentChange(content)
        }

        onContentChange(initialContent)

        onDispose {
            editor.dispose()
        }
    }
}
