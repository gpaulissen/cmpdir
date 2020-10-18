package MyFile;

use File::Basename qw(fileparse);
    
use Moose;
use namespace::autoclean;

has 'filename' => ( is => 'ro', isa => 'Str', required => 1 );

has 'size' => ( is => 'ro', isa => 'Int', required => 1 );
    
has 'blksize' => ( is => 'ro', isa => 'Str', required => 1 );

has 'origin' => ( is => 'ro', isa => 'Str', required => 1 );

sub basename {
    my $self = shift;
    
    return (fileparse($self->filename))[0];
}

sub dirname {
    my $self = shift;
    
    return (fileparse($self->filename))[1];
}

1;
