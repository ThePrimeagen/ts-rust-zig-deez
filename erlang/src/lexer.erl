-module(tokenizer).
-export([create_token/2, is_letter/1, is_number/1, tokenizer/1]).

-record(token, {type, literal}).

create_token(Type, Literal) ->
    #token{type = Type, literal = Literal}.

is_letter(Character) ->
    case Character of
        $a..$z; $A..$Z; $_ -> true;
        _ -> false
    end.

is_number(Character) ->
    case Character of
        $0..$9 -> true;
        _ -> false
    end.

tokenizer(Input) ->
    tokenizer(1, 1, Input, []).

tokenizer(Position, ReadPosition, Input, Tokens) ->
    case read_char(ReadPosition, Input) of
        {Char, _} when Char == $=; Char == $+; Char == $,; Char == $; Char == $($; Char == $); Char == ${; Char == $} ->
            Token = create_token(atom_to_list(type_from_char(Char)), [Char]),
            tokenizer(ReadPosition + 1, ReadPosition + 1, Input, [Token | Tokens]);

        {Char, _} when is_letter(Char) ->
            Ident = read_ident(Position, ReadPosition, Input),
            Token = case keyword_from_ident(Ident) of
                         undefined -> create_token(ident, Ident);
                         Keyword -> Keyword
                     end,
            tokenizer(Position, ReadPosition, Input, [Token | Tokens]);

        {Char, _} when is_number(Char) ->
            Int = read_int(Position, ReadPosition, Input),
            Token = create_token(int, Int),
            tokenizer(Position, ReadPosition, Input, [Token | Tokens]);

        {_, _} -> % Illegal character
            Token = create_token(illegal, [Char]),
            tokenizer(ReadPosition, ReadPosition, Input, [Token | Tokens]);

        eof ->
            lists:reverse([create_token(eof, "eof") | Tokens])
    end.

read_char(Position, Input) ->
    case Position =< length(Input) of
        true -> {element(Position, Input), Position + 1};
        false -> eof
    end.

read_ident(Position, ReadPosition, Input) ->
    case read_char(ReadPosition, Input) of
        {Char, _} when is_letter(Char) ->
            read_ident(Position, ReadPosition + 1, Input);
        _ ->
            string:substr(Input, Position, ReadPosition - Position)
    end.

read_int(Position, ReadPosition, Input) ->
    case read_char(ReadPosition, Input) of
        {Char, _} when is_number(Char) ->
            read_int(Position, ReadPosition + 1, Input);
        _ ->
            string:substr(Input, Position, ReadPosition - Position)
    end.

type_from_char($=) -> equal;
type_from_char($) -> plus;
type_from_char($,) -> comma;
type_from_char($;) -> semicolon;
type_from_char($() -> lparen;
type_from_char($)) -> rparen;
type_from_char($\n) -> lsquirly;
type_from_char($) -> rsquirly.

keyword_from_ident("fn") -> create_token(function, "fn");
keyword_from_ident("let") -> create_token(let, "let");
keyword_from_ident(_) -> undefined.
