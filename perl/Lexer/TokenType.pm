package TokenType;

use strict;
use warnings;

sub new {
    my $class = shift;
    my ($name, $repr) = @_;
    my $self = {
        name => $name,
        repr => $repr
    };
    bless $self, $class;
    return $self;
};

sub getName {
    my $self = shift;
    return $self->{name};
}

sub getRepr {
    my $self = shift;
    return $self->{repr};
}

1;
