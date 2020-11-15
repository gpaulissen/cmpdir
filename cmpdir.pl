#!/usr/bin/env perl # -*- coding: utf-8 -*-

=pod

=head1 NAME

cmpdir.pl - Compare the files in one or more directories (recursive), merge or analyze the compare STDOUT output. 

=head1 SYNOPSIS

  cmpdir.pl [GLOBAL OPTION...] compare [compare OPTION...] DIRECTORY...

or

  cmpdir.pl [GLOBAL OPTION...] merge FILE...

or

  cmpdir.pl [GLOBAL OPTION...] analyze [analyze OPTION...] FILE...

=head1 DESCRIPTION

=head2 Compare directories

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

=head2 Analyze the compare results

The second Use Case is to compare the output of one or more comparisons.

Every file in the standard output from the compare subcommand is gathered
based on size. If the number of files is not equal to the number of
directories or if not every file is equal the files are shown.

=head1 OPTIONS

=head2 GLOBAL OPTIONS

=over 4

=item B<--help>

This help.

=item B<--unit-test>

Perform a unit test.

=item B<--verbose>

Increase verbosity level.

=back

=head2 compare OPTIONS

=over 4

=item B<--bytes>

The number of bytes to read for the hash check. Defaults to 512.

=item B<--function>

The hash function to use (hash or crc32). Defaults to hash from the internal Perl module B.

=back

=head2 analyze OPTIONS

=over 4

=item B<--threshold>

Only display differences when the size is at least this number. Defaults to 0.

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
use Test::More; # do not know how many in advance

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

my $bytes = undef;
my $r_hash_func = 'hash';
my $unit_test = 0;
my $verbose = 0;
my $subcmd = undef;
my $threshold = 0;

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
sub compare ();
sub merge ($);
sub process_files ($);
sub analyze ($);
sub print_analysis ();
sub trim ($);
sub process_directories ();
sub process ();
sub info (@);
sub quote ($);
sub by_cmp (;$$);
sub crc32 ($;$$);
sub unit_test ();

# MAIN

main();

# SUBROUTINES

sub main () 
{
    process_command_line();

    if ($unit_test) {
        unit_test();
    } elsif ($subcmd eq 'compare') {
        compare();
    } elsif ($subcmd eq 'merge') {
        merge('ARGV');
    } elsif ($subcmd eq 'analyze') {
        analyze('ARGV');        
    }
}

sub process_command_line ()
{
    # Windows FTYPE and ASSOC cause the command 'generate_ddl -h -c file'
    # to have ARGV[0] == ' -h -c file' and number of arguments 1.
    # Hence strip the spaces from $ARGV[0] and recreate @ARGV.
    if ( @ARGV == 1 && $ARGV[0] =~ s/^\s+//o ) {
        @ARGV = split( / /, $ARGV[0] );
    }

    # permute for subcommand handling (opposite of require_order
    Getopt::Long::Configure(qw(permute));

    #
    GetOptions('help' => sub { pod2usage(-verbose => 2) },
               'unit-test' => \$unit_test,
               'verbose+' => \$verbose,
               '<>' => sub {
                   my($arg) = @_;

                   if ($arg =~ m/^-/) {
                       die "Usage error: unhandled option $arg detected in global option section";
                   } elsif ($arg !~ m/^(compare|merge|analyze)$/) {
                       die "usage error: invalid subcommand $arg";
                   } else {
                       $subcmd = $arg;
                       die '!FINISH'; # stop processing arguments 
                   }
               })
        or pod2usage(-verbose => 0);

    pod2usage(-verbose => 0)
        unless ($unit_test or defined($subcmd));

    if (defined($subcmd) && $subcmd eq 'compare') {
        GetOptions('bytes=i' => \$bytes,
                   'function=s' => \$r_hash_func)
            or pod2usage(-verbose => 0);

        $bytes = 512
            unless defined($bytes);
        
        pod2usage(-message => "$0: Bytes ($bytes) must be at least 1. Run with --help option.\n")
            unless $bytes >= 1;

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
    } elsif (defined($subcmd) && $subcmd eq 'analyze') {
        GetOptions('threshold=i' => \$threshold)
            or pod2usage(-verbose => 0);

        pod2usage(-message => "$0: Threshold ($threshold) must be at least 0. Run with --help option.\n")
            unless defined($threshold) && $threshold >= 0;

        foreach my $file (@ARGV) {
            pod2usage(-message => "$0: $file is not a file. Run with --help option.\n")
                unless -f $file;
        }
    }
}

sub compare () {
    process_directories();
    process();
}
    
sub merge ($) {
    process_files($_[0]);
    process();
}

sub process_files ($) {    
    my $fh = shift @_;
    my @pos = ();
    my $part = undef;
    my $prev_file;
    my @dirs;

    my @lines = <$fh>; # slurp

    # preallocate the bucket
    # %files will contain every file (prefixed with -) and every size (never negative)
    keys %files = 2 * scalar(@lines);

    foreach my $line (@lines) {
        chomp $line;

        if ($line eq 'Nr  Origin') {
            ;
        } elsif ($line eq '--  ------') {
            @pos = ( [0, 2], [4, undef] );
            $part = 1;
            @dirs = ();
        } elsif ($line eq 'Eq           Size  Origin  Modification time    Filename') {
            ;
        } elsif ($line eq '--           ----  ------  -----------------    --------') {
            @pos = ( [0, 2], [4, 13], [19, 6], [27, 19], [48, undef] );            
            $part = 2;
        } elsif ($line eq 'Compare                  Count  Cached') {
            ;
        } elsif ($line eq '-------                  -----  ------') {
            @pos = ();        
        } elsif (scalar(@pos) > 0&& length($line) > 0) {
            info("line:", $line);
            
            my @cols = ();

            foreach my $pos (@pos) {
                my ($offset, $length) = ($pos->[0], $pos->[1]);

                push(@cols, trim( defined($length) ? substr($line, $offset, $length) : substr($line, $offset) ));
            }

            if ($verbose > 0) {
                $line = '';
            
                for my $i (0..$#cols) {
                    $line .= "[$i] '$cols[$i]' ";
                }
                
                info("pos:", $line);
            }

            if ($part == 1) {
                # store the origin
                my ($index, $origin) = @cols;
                
                $dirs[$index] = $origin;
                
                if (!exists($dirs{$origin})) {
                    $dirs{$origin} = (keys %dirs) + 1;
                }
            } else {
                my ($equal, $size, $origin, $mtime, $filename) = (($cols[0] eq '=='), $cols[1], $dirs[$cols[2]], $cols[3], $cols[4]);
                my $file;

                if ($size eq '') {
                    $size = $prev_file->size;
                }

                $filename = "$origin/$filename";
                
                # check duplicates
                if (!exists($files{'-' . $filename})) {
                    my $hash;
                    
                    # take the hash of the predecessor if the files are equal
                    if ($equal) {
                        $hash = $prev_file->hash;
                    } else {
                        $hash = keys %files; # just a unique number
                    }

                    die "\$hash undefined"
                        unless defined($hash);
                    
                    $file = MyFile->new(filename => $filename, size => $size, mtime => $mtime, origin => $origin, hash => $hash);

                    die "\$file->hash undefined"
                        unless defined($file->hash);
                    

                    $files{$size} = []
                        unless exists $files{$size};

                    $files{'-' . $filename} = $file;
                    
                    push(@{$files{$size}}, $file);
                    
                    info("added file", quote($filename), 'with hash', $file->hash);
                } else {
                    $file = $files{'-' . $filename};

                    die "Size of $filename has changed from $file->size to $size."
                        unless $file->size == $size;
                }
                
                $prev_file = $file;
            }
        }
    }
}

sub analyze ($) {
    process_files($_[0]);
    print_analysis();
}

sub print_analysis () {
}

sub trim ($) {
    my $str = shift @_;

    $str =~ s/^\s+|\s+$//g;

    return $str;
}

sub process_directories ()
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
        info("scanning directory", quote($dir));
        
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

                push(@{$files{$size}}, MyFile->new(filename => $filename, size => $size, mtime => strftime('%Y-%m-%d %H:%M:%S', localtime($mtime)), origin => $origin));
            
                info("added file", quote($filename));
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
            info("display file", ++$file_nr, "/", $nr_files);
        
            ($origin, $filename, $mtime) = ($dirs{$file->origin}, $file->filename, $file->mtime);

            $eq = (defined($prev_file) && by_cmp($file, $prev_file) == 0 ? '==' : '');

            # strip origin from filename
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

sub info (@)
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
    
    info('compare', quote($b->filename), 'with', quote($a->filename), ': [', $retval, $cache, ']');
    
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

sub unit_test () {
    $threshold = 9;

    process_files('DATA');
    
    my ($rh_folders, $rh_files) = (\%dirs, \%files);
    my $rh;
    my @sizes = grep(!/^-/, keys %$rh_files);

    plan tests => 2 + 2 * scalar(@sizes);

    ok( keys %$rh_folders == 2, 'number of folders should be 2' );

    ok( @sizes == 16, 'number of files ' . scalar(@sizes) . ' should be 16' ); # threshold of 9

    foreach my $size (sort { int($b) <=> int($a) } @sizes) {
        my $rh = $rh_files->{$size};
        my $file_count_act = scalar(@$rh);
        my $file_count_exp;

        if ( $size =~ m/^(92583948|92331344|50316|5858|4485|1193|1047)$/ ) {
            $file_count_exp = 1;
        } else {
            $file_count_exp = 2;
        }

        ok( $file_count_act == $file_count_exp, "number of files with size $size ($file_count_act) should be $file_count_exp" );

        my $equal_act = 1;
        my $equal_exp = ( $size =~ m/^(32768|16384|82|8)$/ ? $file_count_act - 1 : $file_count_act );

        info("file 0:", $rh->[0]->str());
        
        for my $i (1..scalar(@$rh)-1) {
            info("file $i:", $rh->[$i]->str());
            
            $equal_act++
                if $rh->[$i]->hash() eq $rh->[0]->hash();
        }

        ok( $equal_act == $equal_exp, "number of equal files with size $size ($equal_act) should be $equal_exp" );
    }

    done_testing();
}

# music.txt

__DATA__


Nr  Origin
--  ------
1   /Volumes/AirPort Disk/Apple Music
2   /Volumes/iTunes/Music

Eq           Size  Origin  Modification time    Filename
--           ----  ------  -----------------    --------
         92583948       1  2020-11-01 22:00:38  Music/Johnny Hallyday/Bercy 90/2-06 Aimer vivre (Live à Bercy _ 1990).m4a
         92331344       2  2018-09-30 11:46:44  Music/Media.localized/Music/Johnny Hallyday/Bercy 90/2-06 Aimer vivre (Live à Bercy _ 1990).m4a
         83094530       1  2014-10-12 13:32:30  Music/Led Zeppelin/Celebration Day [Live] [Disc 1]/1-04 In My Time Of Dying.m4a
==                      2  2014-10-12 13:32:30  Music/Media.localized/Music/Led Zeppelin/Celebration Day [Live] [Disc 1]/1-04 In My Time Of Dying.m4a
         79516345       1  2014-10-12 13:32:48  Music/Led Zeppelin/Celebration Day [Live] [Disc 2]/2-02 Dazed And Confused.m4a
==                      2  2014-10-12 13:32:48  Music/Media.localized/Music/Led Zeppelin/Celebration Day [Live] [Disc 2]/2-02 Dazed And Confused.m4a
         75972781       1  2014-10-12 13:35:12  Music/Led Zeppelin/Led Zeppelin Remasters [Disc 2]/2-09 Achilles Last Stand.m4a
==                      2  2014-10-12 13:35:12  Music/Media.localized/Music/Led Zeppelin/Led Zeppelin Remasters [Disc 2]/2-09 Achilles Last Stand.m4a
         75455261       1  2014-10-12 13:46:34  Music/Scorpions/Tokyo tapes/09 Fly to the rainbow.m4a
==                      2  2014-10-12 13:46:34  Music/Media.localized/Music/Scorpions/Tokyo tapes/09 Fly to the rainbow.m4a
         74990074       1  2014-10-08 22:14:42  Music/Blue Oyster Cult/Extraterrestial/1-07 Roadhouse blues.m4a
==                      2  2014-10-08 22:14:42  Music/Media.localized/Blue Oyster Cult/Extraterrestial/1-07 Roadhouse blues.m4a
            50316       2  2020-10-26 15:48:50  Music/iTunes Music Library.xml
            32768       2  2020-09-27 15:34:17  Music/Music Library.musiclibrary/Genius.itdb
                        1  2020-11-01 10:58:21  Music/Music Library.musiclibrary/Genius.itdb
            16384       2  2020-10-26 12:30:00  Music/Music Library.musiclibrary/Extras.itdb
                        1  2020-11-01 21:48:01  Music/Music Library.musiclibrary/Extras.itdb
             5858       1  2020-11-01 22:14:35  Music/Music Library.musiclibrary/Application.musicdb
             4485       2  2020-10-26 12:29:32  Music/Music Library.musiclibrary/Application.musicdb
             1193       1  2020-11-01 22:06:20  Music/Music Library.musiclibrary/Library Preferences.musicdb
             1047       2  2020-10-26 09:24:21  Music/Music Library.musiclibrary/Library Preferences.musicdb
               82       2  2020-10-26 09:24:42  Music/Music Library.musiclibrary/Preferences.plist
                        1  2020-11-01 21:47:22  Music/Music Library.musiclibrary/Preferences.plist
                8       1  2020-11-01 22:14:36  Music/Music Library.musiclibrary/sentinel
                        2  2020-10-26 15:42:54  Music/Music Library.musiclibrary/sentinel

Compare                  Count  Cached
-------                  -----  ------
==                        4408    2204
!= size                      0       0
!= hash                    122      42
!= file compare              0       0
