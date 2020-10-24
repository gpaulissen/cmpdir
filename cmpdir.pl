#!/usr/bin/env perl

=pod

=head1 NAME

cmpdir.pl - Compare the files in one or more directories (recursive). 

=head1 SYNOPSIS

  cmpdir.pl [OPTION...] DIRECTORY...

=head1 DESCRIPTION

First Use Case is to compare two iTunes Libraries.

The idea is to display the following report:

=over 4

=item Is the file equal to the previous one displayed?

Indicated by two equal signs (==).

=item The file size.

Sorted descending.

=item The last file modify time.

=item The (number of the) original directory.

The number of the directory in the command line DIRECTORY...

=item The full pathname of each file.

=back

=head1 OPTIONS

=over 4

=item B<--help>

This help.

=item B<--verbose>

Increase verbosity level.

=back

=head1 NOTES

The comparison between two files is made by:

=over 4

=item File size

  file1 size <=> file2 size

=item crc32

If the file sizes are equal, the first 512 bytes of each files are read and the crc32 is calculated.

  file1 crc32 <=> file2 crc32

=item File::Compare::compare

If the file sizes and crc32s are equal:

  File::Compare::compare(file1, file2)

=back

=head1 EXAMPLES

=head1 BUGS

=head1 SEE ALSO

The crc32 algorithm is from L<http://billauer.co.il/blog/2011/05/perl-crc32-crc-xs-module/>.

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

use Benchmark qw(:all) ;
use English;
use File::Spec;
use File::Basename;
use File::Find::Rule;
use Getopt::Long;
use Pod::Usage;
use POSIX qw(strftime);

use lib &dirname($0); # to find File::Copy::Recursive in this directory
use MyFile;

# VARIABLES

my $program = &basename($0);
my %dirs;
my %files;
my @cmp = (0, 0, 0, 0, 0);
my @descr = ( '==             ',
              '!= before      ',
              '!= size        ',
              '!= crc32       ',
              '!= file compare'
            );

# command line options

my $verbose = 0;

# FORMATS

my ($nr, $origin, $eq, $size, $mtime, $filename, $descr, $cmp);

format ORIGIN_TOP =
Nr  Origin
--  ------
.

format ORIGIN =
@<  @*
$nr, $origin
.

format FILE_TOP =
Eq           Size  Origin  Modification time    Filename
--           ----  ------  -----------------    --------
.

format FILE =
@<  @>>>>>>>>>>>>  @>>>>>  @<<<<<<<<<<<<<<<<<<  @*
$eq, $size, $origin, $mtime, $filename
.

format COMPARE_TOP =
Compare                  Count
-------                  -----
.

format COMPARE =
@<<<<<<<<<<<<<<  @>>>>>>>>>>>>
$descr, $cmp
.


# PROTOTYPES

sub main ();
sub process_command_line ();
sub process ();
sub log (@);
sub quote ($);

                                                         
# MAIN

main();

# SUBROUTINES

sub main () 
{
    process_command_line();
    timethese(1, { '1 - prepare' => \&prepare, '2 - process' => \&process }, 'nop');
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

    pod2usage(-message => "$0: Must supply at least one directory. Run with --help option.\n")
        unless @ARGV >= 1;

    foreach my $dir (@ARGV) {
        pod2usage(-message => "$0: $dir is not a directory. Run with --help option.\n")
            unless -d $dir;
    }
}

sub prepare ()
{
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
        &log("scanning directory", quote($dir));
        
        # store the origin
        my $origin = File::Spec->rel2abs($dir);

        if (!exists($dirs{$origin})) {
            $dirs{$origin} = (keys %dirs) + 1;
        }            

        foreach my $filename ($rule->in($origin)) {
            my ($size, $mtime, $blksize) = (stat($filename))[7, 9, 11];

            $blksize = 512 unless defined($blksize) && $blksize ne "";    

            $files{$filename} = MyFile->new(filename => $filename, size => $size, mtime => $mtime, blksize => $blksize, origin => $origin)
                unless exists $files{$filename};
            
            &log("added file", quote($filename));
        }
    }
}

sub process ()
{
    my $ofh;

    print "\n";

    $FORMAT_FORMFEED = "";
    $FORMAT_LINES_LEFT = 0;

    $ofh = select(STDOUT);
    $^ = "ORIGIN_TOP";
    $~ = "ORIGIN";
    
    foreach my $dir (sort { $dirs{$a} <=> $dirs{$b} } keys %dirs) {
        ($nr, $origin) = ($dirs{$dir}, $dir);
            
        write;
    }
    
    select($ofh);

    print "\n";
    
    my $prev_file = undef;

    $FORMAT_LINES_LEFT = 0;

    $ofh = select(STDOUT);
    $^ = "FILE_TOP";
    $~ = "FILE";
    # just print one page by setting page length large enough
    $= = (keys %files) + 2;

    my ($file_nr, $nr_files) = (0, scalar(keys %files));
    foreach my $file (reverse sort { $_ = $a->cmp($b); $cmp[abs($_)]++; &log('compare', quote($a->filename), 'with', quote($b->filename), '=', $_); return $_; } values %files) {
        &log("display file", ++$file_nr, "/", $nr_files);
        
        ($size, $origin, $filename, $mtime) = ($file->size, $dirs{$file->origin}, $file->filename, strftime('%Y-%m-%d %H:%M:%S', localtime($file->mtime)));

        my $cmp = (defined $prev_file ? $file->cmp($prev_file) : -1);
        
        $eq = ($cmp != 0 ? '' : '==');
        
        write;
        
        $prev_file = $file;
    }
    select($ofh);

    print "\n";
    
    $FORMAT_LINES_LEFT = 0;

    $ofh = select(STDOUT);
    $^ = "COMPARE_TOP";
    $~ = "COMPARE";
    $= = scalar(@cmp) + 2;

    for my $i (0..$#cmp) {
        ($descr, $cmp) = ($descr[$i], $cmp[$i]);
        write;
    }
    select($ofh);

    print "\n";
}

sub log (@)
{
    return unless $verbose > 0;

    print STDERR "@_\n";
}

sub quote ($)
{
    return "\"$_[0]\"";
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


