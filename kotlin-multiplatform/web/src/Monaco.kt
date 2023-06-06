package monkeylang.web

import androidx.compose.runtime.NoLiveLiterals
import org.w3c.dom.HTMLElement

external class MonacoWrapper {
    val editor: MonacoEditor
}

external interface MonacoEditor {
    fun create(element: HTMLElement, options: MonacoEditorOptions): IStandaloneCodeEditor
}

external interface IStandaloneCodeEditor {
    // https://microsoft.github.io/monaco-editor/docs.html#functions/editor.getModel.html
    fun getModel(): ITextModel

    // https://microsoft.github.io/monaco-editor/docs.html#interfaces/editor.IStandaloneCodeEditor.html#onDidChangeModelContent
    fun onDidChangeModelContent(callback: (IModelContentChangedEvent) -> Unit)

    // https://microsoft.github.io/monaco-editor/docs.html#interfaces/editor.IStandaloneCodeEditor.html#dispose
    fun dispose()
}

// https://microsoft.github.io/monaco-editor/docs.html#interfaces/editor.IModelContentChangedEvent.html
external interface IModelContentChangedEvent

external interface ITextModel {
    // https://microsoft.github.io/monaco-editor/docs.html#interfaces/editor.ITextModel.html#getLinesContent
    fun getLinesContent(): Array<String>
}

// https://microsoft.github.io/monaco-editor/docs.html#interfaces/editor.IStandaloneEditorConstructionOptions.html
external interface MonacoEditorOptions {
    val value: String
    val theme: String
    val automaticLayout: Boolean
}

@NoLiveLiterals
fun MonacoEditorOptions(
    value: String,
    theme: String? = null,
    automaticLayout: Boolean = true,
): MonacoEditorOptions {
    val o = js("({})")
    o["value"] = value
    o["theme"] = theme
    o["automaticLayout"] = automaticLayout
    return o
}

val Monaco: MonacoWrapper = js("require('monaco-editor')")
