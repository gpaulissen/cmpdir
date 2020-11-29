package MyFile;

use File::Basename qw(fileparse);
use File::Compare;

use Moose;
use namespace::autoclean;

has 'filename' => ( is => 'ro', isa => 'Str', required => 1 );

has 'size' => ( is => 'ro', isa => 'Int', required => 1 );
    
has 'mtime' => ( is => 'ro', isa => 'Str', required => 1 );

has 'origin' => ( is => 'ro', isa => 'Str', required => 1 );

has 'hash' => ( is => 'rw', isa => 'Str', required => 0 );

my %compare_results; # cache cmp() results

sub str {
    my ($self) = @_;

    my @info = ('filename:', $self->filename, '; size:', $self->size, '; mtime:', $self->mtime, '; origin:', $self->origin, '; hash:', $self->hash);

    return "@info";
}

sub cmp {
    my ($self, $file, $r_hash_func, $max_bytes_to_read) = @_;

    my ($retval, $cache) = (undef, 1);

    if (exists($compare_results{$self}{$file})) {
        $retval = $compare_results{$self}{$file};
    } elsif (exists($compare_results{$file}{$self})) {
        $retval = -$compare_results{$file}{$self};
    } else {    
        $cache = 0;

        # a simple test which improves performance a lot
        $retval = ($self->filename cmp $file->filename);

        if ($retval != 0) {
            $retval = 1 * ($self->size <=> $file->size);

            if ($retval == 0) {
                die "\$max_bytes_to_read is undefined"
                    unless (defined($max_bytes_to_read) || (defined($self->hash) && defined($file->hash)));
            
                if (!defined($self->hash)) {
                    $self->hash($self->calculate($r_hash_func, $max_bytes_to_read));
                }
    
                if (!defined($file->hash)) {
                    $file->hash($file->calculate($r_hash_func, $max_bytes_to_read));
                }

                # From both files the same number of bytes has been read
                # so now we can deduce that if cmp returns not 0 that the files are not equal.
                $retval = 2 * ($self->hash cmp $file->hash);

                # Only compare files in compare modus
                if ($retval == 0 && defined($max_bytes_to_read)) {
                    $retval = 3 * (File::Compare::compare($self->filename, $file->filename) <=> 0); # something like sign
                }
            }
        }

        $compare_results{$self}{$file} = $retval;
    }

    # $retval:
    # 0        == size, hash and file compare equal
    # -1 / +1  != size
    # -2 / +2  != hash
    # -3 / +3  != file compare
    die "$retval"
        unless abs($retval) <= 3;
    die "$cache"
        unless $cache == 0 || $cache == 1;
    
    return $retval, $cache;
}

sub calculate {
    my ($self, $r_hash_func, $max_bytes_to_read) = @_;
    
    open(F, "< ", $self->filename)
        or die(sprintf("can't open %s: $!\n", $self->filename)); # for Emacs: '

    $max_bytes_to_read = $self->size
        if $self->size < $max_bytes_to_read;
    
    my $input;
    my $bytes = sysread(F, $input, $max_bytes_to_read);    

    die(sprintf("can't read %d bytes from %s: $!\n", $max_bytes_to_read, $self->filename)) # for Emacs: '
        unless defined($bytes) && $bytes == $max_bytes_to_read;

    close F;

    return $r_hash_func->($input);
}

1;
