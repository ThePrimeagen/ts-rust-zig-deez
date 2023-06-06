package monkeylang.web

import org.jetbrains.compose.web.css.* // ktlint-disable no-wildcard-imports

object DebugStyleSheet : StyleSheet() {
    val backgroundColor = Color("#1e1e1e")
    val defaultBorderColor = Color("#010102")

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

    val layout by style {
        display(DisplayStyle.Grid)
        width(100.percent)
        height(100.percent)
        padding(16.px)
        gap(16.px)
        gridTemplateColumns("1fr 1fr")
    }

    val panel by style {
        display(DisplayStyle.Block)
        border {
            width(1.px)
            style(LineStyle.Solid)
            color(defaultBorderColor)
        }
    }

    val debugTokenContainer by style {
        display(DisplayStyle.Flex)
        flexWrap(FlexWrap.Wrap)
        gap(8.px)
        padding(8.px)
    }

    val debugToken by style {
        backgroundColor(Color("#2b2b2b"))
        color(Color("#FFFFFF"))
        padding(4.px, 8.px)
    }

    val debugTokenRange by style {
        color(Color("#888888"))
    }
}
