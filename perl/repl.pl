use Term::ReadLine;
use lib 'Lexer';
use Lexer;
use Token;

my $term = Term::ReadLine->new('MonkeyLang Token REPL');

# Disable prompt underline
$term->ornaments(0); 

while (defined(my $input = $term->readline('> '))) {
    my $lexer = Lexer->new($input);

    my $token = $lexer->nextToken();
    while($token->getType() ne Token->EOF) {
        my $tokenType = $token->getType()->getName();
        my $lit = $token->getLiteral();
        print "$tokenType: $lit";
        print "\n";
        $token = $lexer->nextToken();
    }
}

