package dev.hermannm.monkeylang.web

import org.jetbrains.compose.web.css.Style
import org.jetbrains.compose.web.renderComposable

fun main() {
    renderComposable(rootElementId = "root") {
        Style(ReplStyleSheet)
        ReplConsole()
    }
}
