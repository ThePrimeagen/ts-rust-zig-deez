// ignore_for_file: non_constant_identifier_names

const stringNull = '\u0000';
const whitespace = ' \t\n\r';

/// This is cmpiler optimization & ugly code
const digits = <String>{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};
const alphas = <String>{
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
  'g',
  'h',
  'i',
  'j',
  'k',
  'l',
  'm',
  'n',
  'o',
  'p',
  'q',
  'r',
  's',
  't',
  'u',
  'v',
  'w',
  'x',
  'y',
  'z',
  'A',
  'B',
  'C',
  'D',
  'E',
  'F',
  'G',
  'H',
  'I',
  'J',
  'K',
  'L',
  'M',
  'N',
  'O',
  'P',
  'Q',
  'R',
  'S',
  'T',
  'U',
  'V',
  'W',
  'X',
  'Y',
  'Z',
  '_'
};

bool isAlphy(String val) {
  return val.length == 1 && alphas.contains(val);
}

bool isDiggy(String val) {
  return val.length == 1 && digits.contains(val);
}

extension StringExt on String {
  bool isAlpha() {
    return isAlphy(this);
  }

  bool isDigit() {
    return isDiggy(this);
  }

  bool isWhitespace() => whitespace.contains(this);

  bool operator >=(String other) {
    return compareTo(other) >= 0;
  }

  bool operator <=(String other) {
    return compareTo(other) <= 0;
  }
}
