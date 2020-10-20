#!/usr/bin/env perl

=pod

=head1 NAME

cmpdir.pl - Compare the files in one or more directories (recursive). 

=head1 SYNOPSIS

  cmpdir.pl [OPTION...] DIRECTORY...

=head1 DESCRIPTION

First Use Case is to compare two iTunes Libraries.

The idea is to display the following information:

=over 4

=item The file size

Sorted descending.

=item The equality group.

Ascending. Every file in the same equality group is equal.

=item The original directory.

=item The full pathname of each file excluding the directory on the comand line.

=back

=head1 OPTIONS

=over 4

=item B<--help>

=back

This help.

=head1 NOTES

=head1 EXAMPLES

=head1 BUGS

=head1 SEE ALSO

=head1 AUTHOR

Gert-Jan Paulissen, E<lt>gert.jan.paulissen@gmail.comE<gt>.

=head1 VERSION

=head1 HISTORY

2020-10-18  G.J. Paulissen

First version.

=cut

use 5.008; # Perl 5.8 should be OK

# use autodie; # automatically die when a system call gives an error (for example open)
use strict;
use warnings;

use Data::Dumper;

use File::Spec;
use File::Basename;
use File::Find::Rule;
use Getopt::Long;
use Pod::Usage;

use lib &dirname($0); # to find File::Copy::Recursive in this directory
use MyFile;

# VARIABLES

my $program = &basename($0);
my %dirs;
my %files;

# command line options

my $verbose = 0;

# FORMATS

my ($eq, $size, $origin, $filename);

format FILE_TOP =

Eq           Size  Origin  Filename
--  -------------  ------  ---------------------------------------------------------------------------------------------------------
.

format FILE =
@<  @>>>>>>>>>>>>  @>>>>>  @*
$eq, $size, $origin, $filename
.


# PROTOTYPES

sub main ();
sub process_command_line ();
sub process ();
                                                         
# MAIN

main();

# SUBROUTINES

sub main () 
{
    process_command_line();

    my $rule = File::Find::Rule->new;

    $rule->or( $rule->new
               ->directory
               # ignore hidden (Unix) directories
               ->name('.?*')
               ->prune
               ->discard
             , $rule->new
               ->file
               # ignore hidden (Unix) files
               ->not_name('.?*'));

    foreach my $dir (@ARGV) {
        # store the origin
        my $origin = File::Spec->rel2abs($dir);

        if (!exists($dirs{$origin})) {
            $dirs{$origin} = (keys %dirs) + 1;
        }            

        foreach my $filename ($rule->in($origin)) {
            my ($size, $blksize) = (stat($filename))[7, 11];

            $blksize = 512 unless defined($blksize) && $blksize ne "";    

            $files{$filename} = MyFile->new(filename => $filename, size => $size, blksize => $blksize, origin => $origin)
                unless exists $files{$filename};
        }
    }

    process();
}

sub process_command_line ()
{
    # Windows FTYPE and ASSOC cause the command 'generate_ddl -h -c file'
    # to have ARGV[0] == ' -h -c file' and number of arguments 1.
    # Hence strip the spaces from $ARGV[0] and recreate @ARGV.
    if ( @ARGV == 1 && $ARGV[0] =~ s/^\s+//o ) {
        @ARGV = split( / /, $ARGV[0] );
    }
    
    Getopt::Long::Configure(qw(require_order));

    #
    GetOptions('help' => sub { pod2usage(-verbose => 2) },
               'verbose+' => \$verbose
        )
        or pod2usage(-verbose => 0);
}

sub process ()
{
    my $prev_file = undef;

    select(STDOUT);
    $^ = "FILE_TOP";
    $~ = "FILE";
    # just print one page by setting page length large enough
    $= = (keys %files) + 2;
    
    foreach my $file (reverse sort { $a->cmp($b) } values %files) {
        ($size, $origin, $filename) = ($file->size, $dirs{$file->origin}, $file->filename);

        my $cmp = (defined $prev_file ? $file->cmp($prev_file) : -1);
        
        $eq = ($cmp != 0 ? "" : "=");
        
        write;
        
        $prev_file = $file;
    }
}


__DATA__

stat($filename) returns:

 0 dev      device number of filesystem
 1 ino      inode number
 2 mode     file mode  (type and permissions)
 3 nlink    number of (hard) links to the file
 4 uid      numeric user ID of file's owner
 5 gid      numeric group ID of file's owner
 6 rdev     the device identifier (special files only)
 7 size     total size of file, in bytes
 8 atime    last access time in seconds since the epoch
 9 mtime    last modify time in seconds since the epoch
10 ctime    inode change time in seconds since the epoch (*)
11 blksize  preferred I/O size in bytes for interacting with the
            file (may vary from file to file)
12 blocks   actual number of system-specific blocks allocated
            on disk (often, but not always, 512 bytes each)


file object:

size - returned by stat()
blksize - returned by stat()
directory - command line
pathname
basename


