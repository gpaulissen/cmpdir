package MyFile;

use File::Basename qw(fileparse);
use File::Compare;

use Moose;
use namespace::autoclean;

has 'filename' => ( is => 'ro', isa => 'Str', required => 1 );

has 'size' => ( is => 'ro', isa => 'Int', required => 1 );
    
has 'mtime' => ( is => 'ro', isa => 'Int', required => 1 );

has 'blksize' => ( is => 'ro', isa => 'Str', required => 1 );

has 'origin' => ( is => 'ro', isa => 'Str', required => 1 );

has 'crc32' => ( is => 'rw', isa => 'Int', required => 0 );

my %compare_results; # cache cmp() results

sub cmp {
    my $self = shift;
    my $file = shift;

    my $retval;

    if (exists($compare_results{$self}{$file})) {
        $retval = $compare_results{$self}{$file};
    } elsif (exists($compare_results{$file}{$self})) {
        $retval = -1 * $compare_results{$file}{$self};
    } else {
        $retval = ($self->size <=> $file->size);

        if ($retval == 0) {
            if (!defined($self->crc32)) {
                $self->calculate_crc32;
            }
    
            if (!defined($file->crc32)) {
                $file->calculate_crc32;
            }

            $retval = ($self->crc32 <=> $file->crc32);

            if ($retval == 0) {
                $retval = File::Compare::compare($self->filename, $file->filename);
            }
        }

        $compare_results{$self}{$file} = $retval;
    }

    return $retval;
}

sub calculate_crc32 {
    my $self = shift @_;
    
    open(F, "< ", $self->filename)
        or die(sprintf("can't open %s: $!\n", $self->filename)); # for Emacs: '

    my $input;
    my $bytes = sysread(F, $input, $self->blksize);

    die(sprintf("can't read %s: $!\n", $self->filename)) # for Emacs: '
        unless defined($bytes);

    close F;
            
    # http://billauer.co.il/blog/2011/05/perl-crc32-crc-xs-module/
    my ($init_value, $polynomial) = @_;

    $init_value = 0 unless (defined $init_value);
    $polynomial = 0xedb88320 unless (defined $polynomial);

    my @lookup_table;

    for (my $i=0; $i<256; $i++) {
        my $x = $i;
        for (my $j=0; $j<8; $j++) {
            if ($x & 1) {
                $x = ($x >> 1) ^ $polynomial;
            } else {
                $x = $x >> 1;
            }
        }
        push @lookup_table, $x;
    }

    my $crc = $init_value ^ 0xffffffff;

    foreach my $x (unpack ('C*', $input)) {
        $crc = (($crc >> 8) & 0xffffff) ^ $lookup_table[ ($crc ^ $x) & 0xff ];
    }
    
    $crc = $crc ^ 0xffffffff;

    $self->crc32($crc);
}

1;
