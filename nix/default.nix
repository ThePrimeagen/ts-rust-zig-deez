let
  inherit (builtins)
    elem
    foldl'
    genList
    head
    mapAttrs
    stringLength
    substring
    tail
    ;

  chars = str: genList (i: substring i 1 str) (stringLength str);

  types = mapAttrs (_: type: { inherit type; });

  whitespace = [ " " "\t" "\r" "\n" ];
  alpha = chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  digit = chars "0123456789";
  identTail = alpha ++ digit ++ [ "_" ];

  puncts = types {
    "!" = "bang";
    "(" = "lparen";
    ")" = "rparen";
    "*" = "asterisk";
    "+" = "plus";
    "," = "comma";
    "/" = "slash";
    ";" = "semicolon";
    "<" = "lessThan";
    "=" = "equal";
    ">" = "greaterThan";
    "{" = "lsquirly";
    "}" = "rsquirly";
  };

  keywords = types {
    "let" = "let";
    "fn" = "function";
  };

  takeWhile = xs: foldl'
    ({ left, right, end }: x:
      if end || ! elem x xs then {
        inherit left;
        right = right ++ [ x ];
        end = true;
      } else {
        left = "${left}${x}";
        inherit right;
        end = false;
      })
    { left = ""; right = [ ]; end = false; };

  lex = xs:
    if xs == [ ] then
      [{ type = "eof"; }]
    else
      lex' (head xs) (tail xs);

  lex' = x: xs:
    if puncts ? ${x} then
      [ puncts.${x} ] ++ lex xs
    else if elem x digit then
      let
        split = takeWhile digit ([ x ] ++ xs);
      in
      [{
        type = "int";
        text = split.left;
      }] ++ lex split.right
    else if elem x alpha then
      let
        split = takeWhile identTail ([ x ] ++ xs);
      in
      [
        keywords.${split.left} or {
          type = "ident";
          text = split.left;
        }
      ] ++ lex split.right
    else if elem x whitespace then
      lex xs
    else
      [{ type = "illegal"; }] ++ lex xs;
in

str: lex (chars str)
