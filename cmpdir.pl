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

=item The (number of the) original directory.

The number of the directory in the command line DIRECTORY...

=item The last file modify time.

On Unix there is no reliable way to determine the creation time so the modification time is used.

In local strftime %Y-%m-%d %H:%M:%S format.

=item The full pathname of each file (excluding the origin).

=back

=head1 OPTIONS

=over 4

=item B<--bytes>

The number of bytes to read for the hash check. Defaults to 512.

=item B<--function>

The hash function to use (hash or crc32). Defaults to hash from the internal Perl module B.

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

=item hash

If the file sizes are equal, the first bytes of each files are read (see option bytes) and the hash is calculated.

  file1 hash cmp file2 hash

=item File::Compare::compare

If the file sizes and hashes are equal:

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

use 5.012;

# use autodie; # automatically die when a system call gives an error (for example open)
use strict;
use warnings;

use Data::Dumper;

use B qw(hash); # Perl internal hash function
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
# each element an array reference to compare result and number of cached results
my @cmp = ( [0, 0], [0, 0], [0, 0], [0, 0] );
my @descr = ( '==             ',
              '!= size        ',
              '!= hash        ',
              '!= file compare'
            );

# command line options

my $bytes = 512;
my $r_hash_func = 'hash';
my $verbose = 0;

# FORMATS

my ($nr, $origin, $eq, $display_size, $mtime, $filename, $descr, $cmp, $cached);

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
$eq, $display_size, $origin, $mtime, $filename
.

format COMPARE_TOP =
Compare                  Count  Cached
-------                  -----  ------
.

format COMPARE =
@<<<<<<<<<<<<<<  @>>>>>>>>>>>>  @>>>>>
$descr, $cmp, $cached
.


# PROTOTYPES

sub main ();
sub process_command_line ();
sub process ();
sub log (@);
sub quote ($);
sub by_cmp (;$$);
sub crc32 ($;$$);

# MAIN

main();

# SUBROUTINES

sub main () 
{
    process_command_line();
    prepare();
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
               'bytes=i' => \$bytes,
               'function=s' => \$r_hash_func,
               'verbose+' => \$verbose
        )
        or pod2usage(-verbose => 0);

    pod2usage(-message => "$0: Bytes ($bytes) must be at least 1. Run with --help option.\n")
        unless defined($bytes) && $bytes >= 1;

    my %hash_func = ( 'hash' => \&hash, 'crc32' => \&crc32 );
    my @hash_func = sort keys %hash_func;

    pod2usage(-message => "$0: Must supply a valid hash function (@hash_func). Run with --help option.\n")
        unless exists $hash_func{$r_hash_func};

    $r_hash_func = $hash_func{$r_hash_func};
    
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

        my @files = $rule->in($origin);

        # preallocate the bucket
        # %files will contain every file (prefixed with -) and every size (never negative)
        keys %files = scalar(keys %files) + 2 * scalar(@files);

        foreach my $filename (@files) {
            my ($size, $mtime) = (stat($filename))[7, 9];

            # check duplicates
            if (!exists($files{'-' . $filename})) {
                $files{'-' . $filename} = 1;
                
                $files{$size} = []
                    unless exists $files{$size};
        
                push(@{$files{$size}}, MyFile->new(filename => $filename, size => $size, mtime => $mtime, origin => $origin));
            
                &log("added file", quote($filename));
            }
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
    
    $FORMAT_LINES_LEFT = 0;

    $ofh = select(STDOUT);
    $^ = "FILE_TOP";
    $~ = "FILE";

    my ($file_nr, $nr_files) = (0, scalar(grep(/^-/, keys %files)));
    
    # just print one page by setting page length large enough
    $= = 2 + $nr_files;

    # reverse size by using $b before $a
    # skip the filename items
    foreach my $size (sort { $b <=> $a } grep(!/^-/, keys %files)) {
        $display_size = $size;

        my $prev_file = undef;
        
        foreach my $file (sort by_cmp values @{$files{$size}}) {
            &log("display file", ++$file_nr, "/", $nr_files);
        
            ($origin, $filename, $mtime) = ($dirs{$file->origin}, $file->filename, strftime('%Y-%m-%d %H:%M:%S', localtime($file->mtime)));

            $eq = (defined($prev_file) && by_cmp($file, $prev_file) == 0 ? '==' : '');

            $filename = substr($filename, length($file->origin)+1);
        
            write;
        
            $prev_file = $file;
            $display_size = ''; # do not repeat the same size
        }
    }
    select($ofh);

    print "\n";
    
    $FORMAT_LINES_LEFT = 0;

    $ofh = select(STDOUT);
    $^ = "COMPARE_TOP";
    $~ = "COMPARE";
    $= = scalar(@cmp) + 2;

    for my $i (0..$#cmp) {
        ($descr, $cmp, $cached) = ($descr[$i], $cmp[$i][0], $cmp[$i][1]);
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

sub by_cmp (;$$)
{
    if (scalar(@_) == 2) {
        ($a, $b) = @_;
    }
    
    my ($retval, $cache) = $b->cmp($a, $r_hash_func, $bytes); # reverse by using $b before $a

    $cmp[abs($retval)][0]++;
    $cmp[abs($retval)][1] += $cache;
    
    &log('compare', quote($b->filename), 'with', quote($a->filename), ': [', $retval, $cache, ']');
    
    return $retval;
}

sub crc32 ($;$$) {    
    # http://billauer.co.il/blog/2011/05/perl-crc32-crc-xs-module/
    my ($input, $init_value, $polynomial) = @_;

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

    return $crc;
}
