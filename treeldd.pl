#!/usr/bin/perl

use warnings;
use strict;

use List::MoreUtils  qw(pairwise);
use Getopt::Long     qw(GetOptions);
use Regexp::List;
use Pod::Usage;
use Readonly;
use English          qw(-no_match_vars);

use Data::Dumper;

####
#### CONSTANTS
####

Readonly my $LD_PATHS      => [ qw{ /lib /usr/lib },
                                $ENV{LD_LIBRARY_PATH} ?
                                split /:/, $ENV{LD_LIBRARY_PATH} : () ];
Readonly my $MISSINGLIB_EX => 'Could not find shared library file:';

Readonly my $READELF_CMD   => 'readelf --dynamic ';
Readonly my $READELF_ERROR => qr/ \A readelf: [ ] Error: [ ] (.*) /xms;

Readonly my $READELF_SHARED_LIBS => qr{ Shared[ ]library:
                                        [ ] [[] ([\w.]*) []] }xms;

Readonly my $PACMAN_CMD    => 'pacman -Qo ';
Readonly my $PACMAN_ERROR  => qr/ \A error: [ ] (.*?) $ /xms;
Readonly my $PACMAN_OWNER  => qr/ is [ ] owned [ ] by [ ] (.*?) [ ] /xms;

Readonly my $IGNORELIB_MATCH => Regexp::List->new->list2re
    ( map { ("lib$_.so" => 1) } split /s+/, << 'END_LIST' );
BrokenLocale anl c cidn crypt dl m nsl nss_compat nss_dns nss_files
nss_hesiod nss_nis nss_nisplus pthread resolv rt c m crypt X11 z bz2
jpeg tiff wind Xt Xext ICE com_err crypt crypto /;
END_LIST


####
#### GLOBALS
####

my $VERBOSE = 0;


####
#### FUNCTIONS
####

#---FUNCTION---
# Usage   : my $owner = get_owner_pkg($sharedlib);
# Params  : sharedlib - name of shared library or an absolute path to a
#                       file
# Returns : The name of the package who owns the file or library,
#           or '?' if none was found.
#--------------

sub get_owner_pkg
{
    my ($sharedlib) = @_;

    my $file_path;
    eval {
        $file_path = ( -e $sharedlib ? $sharedlib
                       : find_sharedlib_path($sharedlib) );
    };

    if ($EVAL_ERROR) {
        die $EVAL_ERROR if ( $EVAL_ERROR !~ /\A$MISSINGLIB_EX/ms );
        return '?';
    }

    my $pacman_output = `$PACMAN_CMD $file_path`;
    if ( $pacman_output =~ $PACMAN_ERROR ) {
        die "pacman command failed: $1\n";
    }

    my ($owner_pkg) = $pacman_output =~ /$PACMAN_OWNER/;
    return $owner_pkg || '?';
}

#---FUNCTION---
# Usage   : my @output_lines = make_tree_display($tree);
# Params  : $tree - Array reference that represents a tree
#           $depth_counts - *Internal Use Only* A stack of item counts at each
#                           depth level, used to display vertical lines.
# Returns : Lines suitable to print to screen, output is a textual tree
#           exactly like the 'tree' command-line program.
#--------------

sub make_tree_display
{
    my ($tree, $depth_counts) = @_;

    $depth_counts ||= [ ];
    my $node_name = $tree->[0];

    my $prefix = '';
    if ( @$depth_counts ) {
        # If there are no more items in a depth above us, they
        # won't need a line to represent their branch.
        for my $i ( 0 .. $#$depth_counts-1 ) {
            my $nodes_on_depth = $depth_counts->[$i];
            $prefix .= ( $nodes_on_depth > 0 ? '|   ' : '    ' );
        }

        # If this is the last item, make a curved "twig".
        my $more_siblings = --$depth_counts->[-1];
        $prefix .= ( $more_siblings ? '|-- ' : '`-- ' );
    }

    my @tree_lines = ( "$prefix$node_name\n" );

    # Recurse through the the children nodes...
    my $child_count = $#$tree;
    for my $i ( 1 .. $#$tree ) {
        push @tree_lines, make_tree_display( $tree->[$i],
                                            [ @$depth_counts,
                                              $child_count-- ] );
    }

    return @tree_lines;
}

#---FUNCTION---
# Usage   : my $abs_lib_path = find_sharedlib_path($lib_filename)
# Purpose : Searches for the library file in standard locations.
# Params  : lib_filename - the library filename (ie: libMagickCore.so.2)
#                          without the leading path
# Returns : The absolute path to the found library.
#--------------

sub find_sharedlib_path
{
    my ($lib_filename) = @_;
    for my $lib_path ( map { $_ . "/$lib_filename" } @{$LD_PATHS} ) {
        return $lib_path if ( -e $lib_path );
    }

    die "$MISSINGLIB_EX $lib_filename\n";
}

#---FUNCTION---
# Usage   : my @needed_libs = readelf_sharedlibs($elf_file)
# Purpose : Reads the shared libraries the ELF binary file depends on to run
# Params  : elf_file - library name or absolute path to an ELF binary
# Returns : Lists of shared library names that the ELF binary depends on.
#--------------

sub readelf_sharedlibs
{
    my ($elf_file) = @_;

    my $elf_filepath = ( -e $elf_file
                         ? $elf_file
                         : find_sharedlib_path($elf_file) );

    my $readelf_output = `$READELF_CMD $elf_filepath`;

    if ( $readelf_output =~ /$READELF_ERROR/ ) {
        die "readelf command failed: $1\n";
    }

    return $readelf_output =~ /$READELF_SHARED_LIBS/g;
}

#---FUNCTION---
# Usage   : my $tree = make_dep_tree( $filepath_or_libname );
# Params  : filepath_or_libname - File or library to become the tree-top.
#           was_checked         - *Internal Use Only* a hashref matching
#                                 files/libs already examined
# Returns : A tree represented as nested array refs.  This tree represents
#           all the dependencies of the program, its dependencies, etc.
#--------------

sub make_dep_tree
{
    my ($file_or_lib, $was_checked) = @_;

    $was_checked = {} unless ( eval { ref $was_checked eq 'HASH' } );

    return ( $VERBOSE ? [ "*$file_or_lib" ] : undef )
        if ( $was_checked->{$file_or_lib} );

    my @needed_libs = eval { readelf_sharedlibs($file_or_lib) };

    $was_checked->{$file_or_lib} = 1;

    # Give up on this path if we can't find a library file...
    if ($EVAL_ERROR) {
        die $EVAL_ERROR if ( $EVAL_ERROR !~ /\A$MISSINGLIB_EX/ );
        return undef;
    }

    # Recurse until we have checked every library...
    my @found_paths;

    NEEDED_LIBRARY:
    for my $sharedlib (@needed_libs) {
        next NEEDED_LIBRARY if ( $sharedlib =~ /$IGNORELIB_MATCH/ );

        my $child_paths = make_dep_tree( $sharedlib, $was_checked );

        if ( defined $child_paths ) {
            push @found_paths, $child_paths;
        }
    }

    return [ $file_or_lib, @found_paths ];
}


####
#### SCRIPT START
####

# Get command lines options and print usage if a problem occurs...
my ($maxdepth, $show_pkgs, $show_help, $show_man);

GetOptions( 'depth=i',  \$maxdepth,
            'packages', \$show_pkgs,
            'verbose',  \$VERBOSE,

            'help',     \$show_help,
            'man',      \$show_man );

my ($binfile, @target_libs) = @ARGV;

pod2usage({ -verbose => 2 }) if ($show_man);
pod2usage({ -verbose => 1 }) if ($show_help);
pod2usage() if (!$binfile);
pod2usage("error: $binfile file not found\n") if ( ! -e $binfile );

# Create our shared library dependency tree...
my %is_target_lib = map { ( $_ => 1 ) } @target_libs;
my $deptree = make_dep_tree( $binfile );

my @output = make_tree_display($deptree);

# Append the names of packages who own the files to each line if requested...
sub tree_to_packages {
    my $libname = $_->[0];
    $libname =~ s/ \A [*] //xms;
    return ( get_lib_owner($libname),
             map &tree_to_packages, @{$_}[ 1 .. $#$_ ] );
}

if ( $show_pkgs ) {
    my @owner_list = map &tree_to_packages, $deptree;

    @output = pairwise { chomp $b; chomp $a; "$a ($b)\n"; }
        @output, @owner_list;
}

print @output;
exit 0;

__END__

=head1 NAME

treeldd - tree & ldd's forbidden lovechild

=head1 SYNOPSIS

treeldd <options> [path to binary] <target libraries>

 Options:
  -h, --help       this brief help message
  -v, --verbose    prints shared libraries that were already displayed
                   previously, prepending them with an asterisk ("*")
  -p, --packages   displays the pacman package which owns the library
                   or file (slows program down a couple seconds)
  -d, --depth=NUM  maximum depth to recurse into when displaying

=head1 DESCRIPTION

Displays a tree diagram representing shared library dependencies.
Target libraries can be specified to limit branches only to those
leading from the elf binary to the provided libraries.

The elf binary starting point can be a shared library or executable.

=head1 SEE ALSO

=over

=item L<tree(1)>

Where the tree display format came from.

=item L<ldd(1)>

Who introduced me to lists of shared libraries.

=item L<readelf(1)>

Who reads the (non-recursive) list of needed shared libraries for us.

=item L<pacman(8)>

Who started this mess in the first place.

=back

=head1 AUTHOR

Justin Davis, C<< jrcd<eighty-three> at gmail dot com >> (convert number to numeric), aka juster on L<bbs.archlinux.org>

=head1 COPYRIGHT & LICENSE

Copyright 2009 Justin Davis, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut
