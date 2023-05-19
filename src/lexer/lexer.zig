const std = @import("std");

const Token = union(enum) {
    ident: []const u8,
    integer: []const u8,
    let: []const u8,

    illegal: void,
    eof: void,
    equal: void,
    plus: void,
    comma: void,
    semicolon: void,
    lparen: void,
    rparen: void,
    lsquirly: void,
    rsquirly: void,
    function: void,
};

pub const Lexer = struct {
    const Self = @This();

    read_position: usize,
    position: usize,
    ch: u8,
    input: []const u8,

    pub fn init(input: []const u8) Self {
        var lex = Self{
            .read_position = 0,
            .position = 0,
            .ch = 0,
            .input = input,
        };

        lex.read_char();

        return lex;
    }

    pub fn next_token(self: *Self) Token {
        const tok = switch (self.ch) {
            '{' => Token{ .lsquirly = {} },
            '}' => Token{ .rsquirly = {} },
            '(' => Token{ .lparen = {} },
            ')' => Token{ .rparen = {} },
            ',' => Token{ .comma = {} },
            ';' => Token{ .semicolon = {} },
            '+' => Token{ .plus = {} },
            '=' => Token{ .equal = {} },
            0 => Token{ .eof = {} },
            else => Token{ .illegal = {} },
        };

        self.read_char();
        return tok;
    }

    fn read_char(self: *Self) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }
};

const expectEqualDeep = std.testing.expectEqualDeep;
test "Lexer" {
    const input = "=+(){},;";
    var lex = Lexer.init(input);

    var tokens = [_]Token{
        Token{ .equal = {} },
        Token{ .plus = {} },
        Token{ .lparen = {} },
        Token{ .rparen = {} },
        Token{ .lsquirly = {} },
        Token{ .rsquirly = {} },
        Token{ .comma = {} },
        Token{ .semicolon = {} },
        Token{ .eof = {} },
    };

    for (tokens) |token| {
        const tok = lex.next_token();

        try expectEqualDeep(token, tok);
    }
}
