package Lexer;

# constructor!
sub new {
    my $class = shift;
    my ($input, $position, $readPosition) = @_;
    my $self = {
        input => $input,
        position => $position // 0,
        readPosition => $readPosition // 0,
        ch => undef,
    };
    # this might be like javascript's bind?
    bless $self, $class;
    $self->readChar();
    return $self;
}

sub skipWhitespace() {
    my $self = shift;
    # space is 32, any number less is a control code,
    # including carrage returns
    while($self->{ch} <= 32 && $self->{ch} > 0) {
        $self->readChar();
    }
}

sub readChar() {
    # this is the masochistic way perl gets ares
    my $self = shift;

    if($self->{readPosition} >= length($self->{input})) {
        $self->{ch} = 0;
    } else {
        $self->{ch} = ord(substr($self->{input}, $self->{readPosition}));
    }

    $self->{position} = $self->{readPosition};
    $self->{readPosition} = $self->{readPosition} + 1;
}

sub peekChar() {
    my $self = shift;
    if($self->{readPosition} >= length($self->{input})) {
        return 0;
    } else {
        return substr($self->{input}, $self->{readPosition}, 1);
    }
}

# this underscore is a private method convention, I hate it too.
sub _isLetter {
    # @_ is the array of args passed into the method
    my ($char) = @_;
    return ($char >= 65 && $char <= 90) || $char == 95 || ($char >= 97 && $char <= 122);
}

sub _isDigit {
    my ($char) = @_;
    return ($char >= 48 && $char <= 57);
}

sub readIdentifier() {
    my $self = shift;
    my $start = $self->{position};
    while(_isLetter($self->{ch})) {
        $self->readChar();
    }
    my $identSize = $self->{position} - $start;
    return substr($self->{input}, $start, $identSize);
}

sub readNumber() {
    my $self = shift;
    my $start = $self->{position};
    while(_isDigit($self->{ch})) {
        $self->readChar();
    }
    my $identSize = $self->{position} - $start;
    return substr($self->{input}, $start, $identSize);
}

sub nextToken() {
    my $self = shift;

    $self->skipWhitespace();

    my $newToken = Token->ILLEGAL;
    my $charNum = $self->{ch};
    my $literal = chr($charNum);

    if($literal eq '=') { 
        if($self->peekChar() eq '=') {
            $self->readChar();
            $literal .= chr($self->{ch});
            $newToken = Token->EQ;
        } else {
            $newToken = Token->ASSIGN;
        }
    }
    elsif($literal eq ';') { $newToken = Token->SEMICOLON; }
    elsif($literal eq '(') { $newToken = Token->LPAREN; }
    elsif($literal eq ')') { $newToken = Token->RPAREN; }
    elsif($literal eq ',') { $newToken = Token->COMMA; }
    elsif($literal eq '+') { $newToken = Token->PLUS; }
    elsif($literal eq '{') { $newToken = Token->LBRACE; }
    elsif($literal eq '}') { $newToken = Token->RBRACE; }
    elsif($literal eq '-') { $newToken = Token->MINUS; }
    elsif($literal eq '!') { 
        if($self->peekChar() eq '=') {
            $self->readChar();
            $literal .= chr($self->{ch});
            $newToken = Token->NOT_EQ;
        } else {
            $newToken = Token->BANG; 
        }
    }
    elsif($literal eq '*') { $newToken = Token->ASTERISK; }
    elsif($literal eq '/') { $newToken = Token->SLASH; }
    elsif($literal eq '<') { $newToken = Token->LT; }
    elsif($literal eq '>') { $newToken = Token->GT; }
    elsif($charNum eq 0) { return Token->new(Token->EOF, ""); }
    elsif(_isLetter($charNum)) {
        $literal = $self->readIdentifier();
        $newToken = Token::lookupIdent($literal);
        return Token->new($newToken, $literal);
    } elsif(_isDigit($charNum)) {
        $literal = $self->readNumber();
        $newToken = Token->INT;
        return Token->new($newToken, $literal);
    } else {
       $newToken = Token->ILLEGAL;
    }

    $self->readChar();

    return Token->new($newToken, $literal);
}

sub toString {
    my $self = shift;
    my $read = substr($self->{input}, $self->{position}, 1);
    my $pos = $self->{position};
    my $readPos = $self->{readPosition};
    return "(pos: $pos, readPos: $readPos) - $read";
}

1;

