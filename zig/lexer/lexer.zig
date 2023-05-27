const std = @import("std");

const Token = union(enum) {
    ident: []const u8,
    int: []const u8,

    let,
    illegal,
    eof,
    equal,
    plus,
    comma,
    semicolon,
    lparen,
    rparen,
    lsquirly,
    rsquirly,
    function,

    fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "fn", .function },
        });
        return map.get(ident);
    }
};

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isInt(ch: u8) bool {
    return std.ascii.isDigit(ch);
}

pub const Lexer = struct {
    const Self = @This();

    read_position: usize = 0,
    position: usize = 0,
    ch: u8 = 0,
    input: []const u8,

    pub fn init(input: []const u8) Self {
        var lex = Self{
            .input = input,
        };

        lex.read_char();

        return lex;
    }

    pub fn next_token(self: *Self) Token {
        self.skip_whitespace();
        const tok: Token = switch (self.ch) {
            '{' => .lsquirly,
            '}' => .rsquirly,
            '(' => .lparen,
            ')' => .rparen,
            ',' => .comma,
            ';' => .semicolon,
            '+' => .plus,
            '=' => .equal,
            0 => .eof,
            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.read_identifier();
                if (Token.keyword(ident)) |token| {
                    return token;
                }
                return .{ .ident = ident };
            },
            '0'...'9' => {
                const int = self.read_int();
                return .{ .int = int };
            },
            else => .illegal,
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

    fn read_identifier(self: *Self) []const u8 {
        const position = self.position;

        while (isLetter(self.ch)) {
            self.read_char();
        }

        return self.input[position..self.position];
    }

    fn read_int(self: *Self) []const u8 {
        const position = self.position;

        while (isInt(self.ch)) {
            self.read_char();
        }

        return self.input[position..self.position];
    }

    fn skip_whitespace(self: *Self) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.read_char();
        }
    }
};

const expectEqualDeep = std.testing.expectEqualDeep;
test "Lexer" {
    const input = "=+(){},;";
    var lex = Lexer.init(input);

    var tokens = [_]Token{
        .equal,
        .plus,
        .lparen,
        .rparen,
        .lsquirly,
        .rsquirly,
        .comma,
        .semicolon,
        .eof,
    };

    for (tokens) |token| {
        const tok = lex.next_token();

        try expectEqualDeep(token, tok);
    }

    // uncomment to print out Token.Tag fields
    // std.log.warn("Token.Tag fields:", .{});
    // inline for (std.meta.fields(Token.Tag)) |f| {
    //     std.log.warn("{s}", .{f.name});
    // }
}

test "Lexer - Full" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
    ;
    var lex = Lexer.init(input);

    var tokens = [_]Token{
        .let,
        .{ .ident = "five" },
        .equal,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .equal,
        .{ .int = "10" },
        .semicolon,
        .let,
        .{ .ident = "add" },
        .equal,
        .function,
        .lparen,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .rparen,
        .lsquirly,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .semicolon,
        .rsquirly,
        .semicolon,
        .let,
        .{ .ident = "result" },
        .equal,
        .{ .ident = "add" },
        .lparen,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .rparen,
        .semicolon,
        .eof,
    };

    for (tokens) |token| {
        const tok = lex.next_token();

        try expectEqualDeep(token, tok);
    }
}
