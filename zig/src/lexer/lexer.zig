const std = @import("std");

const Token = union(enum) {
    ident: []const u8,
    int: []const u8,

    let,
    illegal,
    eof,
    assign,
    plus,
    comma,
    semicolon,
    lparen,
    rparen,
    lsquirly,
    rsquirly,
    function,

    bang,
    dash,
    forward_slash,
    asterisk,
    less_than,
    greater_than,

    equal,
    not_equal,

    if_token,
    else_token,
    return_token,
    false_token,
    true_token,

    fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "fn", .function },
            .{ "if", .if_token },
            .{ "true", .true_token },
            .{ "false", .false_token },
            .{ "return", .return_token },
            .{ "else", .else_token },
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

    pub fn has_tokens(self: *Self) bool {
        return self.ch != 0;
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
            '-' => .dash,
            '/' => .forward_slash,
            '*' => .asterisk,
            '<' => .less_than,
            '>' => .greater_than,
            '!' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk .not_equal;
                } else {
                    break :blk .bang;
                }
            },
            '=' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk .equal;
                } else {
                    break :blk .assign;
                }
            },
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

    fn peek_char(self: *Self) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
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
        .assign,
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
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\10 == 10;
        \\10 != 9;
    ;
    var lex = Lexer.init(input);

    var tokens = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .assign,
        .{ .int = "10" },
        .semicolon,
        .let,
        .{ .ident = "add" },
        .assign,
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
        .assign,
        .{ .ident = "add" },
        .lparen,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .rparen,
        .semicolon,

        .bang,
        .dash,
        .forward_slash,
        .asterisk,
        .{ .int = "5" },
        .semicolon,
        .{ .int = "5" },
        .less_than,
        .{ .int = "10" },
        .greater_than,
        .{ .int = "5" },
        .semicolon,
        .if_token,
        .lparen,
        .{ .int = "5" },
        .less_than,
        .{ .int = "10" },
        .rparen,
        .lsquirly,
        .return_token,
        .true_token,
        .semicolon,
        .rsquirly,
        .else_token,
        .lsquirly,
        .return_token,
        .false_token,
        .semicolon,
        .rsquirly,

        .{ .int = "10" },
        .equal,
        .{ .int = "10" },
        .semicolon,
        .{ .int = "10" },
        .not_equal,
        .{ .int = "9" },
        .semicolon,

        .eof,
    };

    for (tokens) |token| {
        const tok = lex.next_token();

        try expectEqualDeep(token, tok);
    }
}
