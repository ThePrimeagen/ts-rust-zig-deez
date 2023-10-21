package root.util;

import root.LocalizedToken;

public class ExceptionUtil {

    public static String buildMessage(String title, String message, LocalizedToken localizedToken) {
        var lineLenght = localizedToken.codeLine().length();
        var lineNumberString = (localizedToken.line() < 9 ? "0" : "") + (localizedToken.line() + 1);
        var additionalPadding = lineNumberString.length() + 2;

        var lineSummary = new StringBuilder("%s: %s\n".formatted(lineNumberString, localizedToken.codeLine()));

        for (int i = 0; i < lineLenght + additionalPadding; i++) {
            if (i == localizedToken.column() + additionalPadding) {
                lineSummary.append("^");
            } else {
                lineSummary.append("-");
            }
        }

        return "%s: %s\n%s".formatted(title, message, lineSummary.toString());
    }
}
