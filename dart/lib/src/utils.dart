int _c(String c) {
  return c.codeUnitAt(0);
}

extension StringExt on String {
  bool isLetter() {
    final ch = _c(this);
    return (_c("a") <= ch && ch <= _c("z")) ||
        (_c("A") <= ch && ch <= _c("Z")) ||
        ch == _c("_");
  }

  bool isNumber() {
    final ch = _c(this);
    return _c("0") <= ch && ch <= _c("9");
  }

  bool isWhitespace() =>
      this == " " || this == "\t" || this == "\n" || this == "\r";
}
