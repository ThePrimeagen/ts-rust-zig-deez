export const isLetter = (ch: string) =>
  ('a' <= ch && 'z' >= ch) || ('A' <= ch && 'Z' >= ch) || ch === '_';

export const isNumber = (ch: string) => '0' <= ch && '9' >= ch;

export const isWhitespace = (ch: string) =>
  ch === ' ' || ch === '' || ch === '\t' || ch === '\n' || ch === '\r';

export const readIdentifier = (buffer: string) => {
  let pos = 0;
  while (isLetter(buffer[pos])) {
    pos++;
  }

  return buffer.substring(0, pos);
};

export const readNumber = (buffer: string) => {
  let pos = 0;
  while (isNumber(buffer[pos])) {
    pos++;
  }

  return buffer.substring(0, pos);
};

export const skipWhitespace = (input: string) => {
  let pos = 0;
  while (isWhitespace(input[pos])) {
    pos++;
  }

  return input.substring(pos);
};
