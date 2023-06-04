package dev.hermannm.monkeylang.web

import androidx.compose.runtime.* // ktlint-disable no-wildcard-imports
import dev.hermannm.monkeylang.Lexer
import org.jetbrains.compose.web.attributes.InputType
import org.jetbrains.compose.web.attributes.autoFocus
import org.jetbrains.compose.web.attributes.onSubmit
import org.jetbrains.compose.web.attributes.placeholder
import org.jetbrains.compose.web.css.* // ktlint-disable no-wildcard-imports
import org.jetbrains.compose.web.dom.* // ktlint-disable no-wildcard-imports
import org.w3c.dom.HTMLPreElement

@Composable
fun ReplConsole() {
    var input by remember { mutableStateOf("") }
    var buffer by remember { mutableStateOf("") }
    var bufferElement: HTMLPreElement? = null

    fun printLineToBuffer(text: String) {
        buffer += text + "\n"
    }

    fun evaluateInput(input: String) {
        Lexer(input).forEach { token ->
            printLineToBuffer(token.toString())
        }
        buffer += "\n"
    }

    fun scrollBufferToBottom() {
        val element = bufferElement ?: return
        element.scrollTop = 99999999.0
    }

    Div({
        classes(ReplStyleSheet.replLayout)
    }) {
        Pre({
            classes(ReplStyleSheet.replBuffer)
            ref { element ->
                bufferElement = element
                onDispose { bufferElement = null }
            }
        }) {
            Text(buffer)
            DisposableEffect(buffer) {
                // Scroll to bottom on buffer change
                scrollBufferToBottom()
                onDispose { }
            }
        }
        Form(attrs = {
            onSubmit {
                it.preventDefault()
                evaluateInput(input)
                input = ""
            }
        }) {
            Input(InputType.Text) {
                classes(ReplStyleSheet.replInput)
                value(input)
                onInput { input = it.value }
                placeholder("Enter code here...")
                autoFocus()
            }
        }
    }
}

object ReplStyleSheet : StyleSheet() {
    val backgroundColor = Color("#1e1e1e")
    val inputColor = Color("#2A2631")
    val textColor = Color("#FFFFFF")
    val defaultBorderColor = Color("#010102")
    val activeBorderColor = Color("#4F91FF")

    init {
        "body, html, #root" style {
            margin(0.px)
            padding(0.px)
            height(100.percent)
        }

        "body" style {
            fontFamily("monospace")
            fontSize(12.px)
            backgroundColor(backgroundColor)
        }

        "*" style {
            boxSizing("border-box")
        }
    }

    val replLayout by style {
        display(DisplayStyle.Grid)
        width(100.percent)
        height(100.percent)
        padding(16.px)
        gap(16.px)
        gridTemplateRows("1fr auto")
    }

    val replBuffer by style {
        overflowY("auto")
        overflowX("auto")
        margin(0.px)
        backgroundColor(inputColor)
        border {
            width(1.px)
            style(LineStyle.Solid)
            color(defaultBorderColor)
        }
        color(textColor)
        padding(1.em)
    }

    val replInput by style {
        color(textColor)
        width(100.percent)
        padding(1.em)
        backgroundColor(inputColor)
        border {
            width(1.px)
            style(LineStyle.Solid)
            color(defaultBorderColor)
        }

        fontFamily("monospace")
        fontSize(12.px)

        group(self + focus, self + active) style {
            outline("none")
            border {
                width(1.px)
                style(LineStyle.Solid)
                color(activeBorderColor)
            }
        }
    }
}
