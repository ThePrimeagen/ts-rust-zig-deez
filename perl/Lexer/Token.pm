package Token;

use strict;
use warnings;
use TokenType;
use Exporter 'import';

use constant ILLEGAL => TokenType->new("ILLEGAL", "ILLEGAL");
use constant EOF => TokenType->new("EOF", "EOF");
use constant IDENT => TokenType->new("IDENT", "IDENT");
use constant INT => TokenType->new("INT", "INT");
use constant ASSIGN => TokenType->new("ASSIGN", "=");
use constant PLUS => TokenType->new("PLUS", "+");
use constant LPAREN => TokenType->new("LPAREN", "(");
use constant RPAREN => TokenType->new("RPAREN", ")");
use constant LBRACE => TokenType->new("LBRACE", "{");
use constant RBRACE => TokenType->new("RBRACE", "}");
use constant COMMA => TokenType->new("COMMA", ",");
use constant SEMICOLON => TokenType->new("SEMICOLON", ";");
use constant FUNCTION => TokenType->new("FUNCTION", "FUNCTION");
use constant LET => TokenType->new("LET", "LET");
use constant MINUS => TokenType->new("MINUS", "-");
use constant BANG => TokenType->new("BANG", "!");
use constant ASTERISK => TokenType->new("ASTERISK", "*");
use constant SLASH => TokenType->new("SLASH", "/");
use constant LT => TokenType->new("LT", "<");
use constant GT => TokenType->new("GT", ">");
use constant TRUE => TokenType->new("TRUE", "TRUE");
use constant FALSE => TokenType->new("FALSE", "FALSE");
use constant IF => TokenType->new("IF", "IF");
use constant ELSE => TokenType->new("ELSE", "ELSE");
use constant RETURN => TokenType->new("RETURN", "RETURN");
use constant EQ => TokenType->new("EQ", "==");
use constant NOT_EQ => TokenType->new("NOT_EQ", "!=");

sub new {
    my $class = shift;
    my ($type, $literal) = @_;
    my $self = {
        type => $type,
        literal => $literal
    };
    bless $self, $class;
    return $self;
};

sub getType {
    my $self = shift;
    return $self->{type};
};

sub getLiteral {
    my $self = shift;
    return $self->{literal};
};

my %keywords = (
    fn => FUNCTION,
    let => LET,
    true => TRUE,
    false => FALSE,
    if => IF,
    else => ELSE,
    return => RETURN
);

sub lookupIdent {
    my $key = shift;
    return $keywords{$key} // IDENT;
}


our @EXPORT = qw(
    ILLEGAL
    EOF
    IDENT
    INT
    ASSIGN
    PLUS
    LPAREN
    RPAREN
    LBRACE
    RBRACE
    COMMA
    SEMICOLON
    FUNCTION
    LET
    BANG
    ASTERISK
    SLASH
    LT
    GT
    TRUE
    FALSE
    IF
    ELSE
    RETURN
);

1;

