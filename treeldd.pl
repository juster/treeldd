#!/usr/bin/perl

use warnings;
use strict;

use List::MoreUtils  qw(pairwise);
use Getopt::Long     qw(GetOptions);
use Regexp::List;
use Pod::Usage;
use Readonly;
use Memoize;

#use English          qw(-no_match_vars);

####
#### CONSTANTS
####

Readonly my $LD_PATHS      => [ qw{ /lib /usr/lib },
                                $ENV{LD_LIBRARY_PATH} ?
                                split /:/, $ENV{LD_LIBRARY_PATH} : () ];
Readonly my $MISSINGLIB_EX => 'Could not find shared library file:';

Readonly my $READELF_CMD   => 'readelf --dynamic ';
Readonly my $READELF_ERROR => qr/ \A readelf: [ ] Error: [ ] (.*) /xmso;

Readonly my $READELF_SHARED_LIBS => qr{ Shared[ ]library:
                                        [ ] [[] ([\w.]*) []] }xmso;

Readonly my $PACMAN_CMD    => 'pacman -Qo ';
Readonly my $PACMAN_ERROR  => qr/ \A error: [ ] (.*?) $ /xmso;
Readonly my $PACMAN_OWNER  => qr/ is [ ] owned [ ] by [ ] (.*?) [ ] /xmso;

# Ignore libs from packages: glibc gcc-libs crypt
Readonly my $IGNORELIB_LIST => 'lib(?:' .
    join('|', ( sort split /\s+/, << 'END_LIST' )) . ').so';
BrokenLocale anl c cidn crypt dl m nsl nss_compat nss_dns nss_files
nss_hesiod nss_nis nss_nisplus pthread resolv rt c m crypt X11 z bz2
gcc_s gfortran gomp mudflap mudflapth objc ssp stdc++
END_LIST

####
#### GLOBALS
####

my $VERBOSE = 0;
my $IGNORELIB_MATCH = qr/\A$IGNORELIB_LIST/o;
my %DEPS_OF;


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

    if ($@) {
        die $@ if ( $@ !~ /\A$MISSINGLIB_EX/ms );
        return '?';
    }

    my $pacman_output = `$PACMAN_CMD $file_path`;
    if ( $pacman_output =~ $PACMAN_ERROR ) {
        die "pacman command failed: $1\n";
    }

    my ($owner_pkg) = $pacman_output =~ /$PACMAN_OWNER/;
    return $owner_pkg || '?';
}

memoize('get_owner_pkg');

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
    my ($tree, $maxdepth, $depth_counts) = @_;

    return () if $maxdepth-- < 0;

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
        push @tree_lines, make_tree_display( $tree->[$i], $maxdepth,
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
memoize('find_sharedlib_path');

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

    return grep { $_ !~ /$IGNORELIB_MATCH/ }
        $readelf_output =~ /$READELF_SHARED_LIBS/g;
}
memoize('readelf_sharedlibs');

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
    my ($file_or_lib, $node_of) = @_;

    $node_of = {} unless ( eval { ref $node_of eq 'HASH' } );

    return $node_of->{$file_or_lib} if ( exists $node_of->{$file_or_lib} );

    my $new_node = [ $file_or_lib ];
    $node_of->{$file_or_lib} = $new_node;

    my @needed_libs = eval { readelf_sharedlibs($file_or_lib) };

    # Give up on this path if we can't find a library file...
    if ($@) {
        die $@ if ( $@ !~ /\A$MISSINGLIB_EX/ );
        return undef;
    }

    # Recurse until we have checked every library...
    my @found_paths;

    NEEDED_LIBRARY:
    for my $sharedlib (@needed_libs) {
        next NEEDED_LIBRARY if ( $VERBOSE < 2 &&
                                 $sharedlib =~ /$IGNORELIB_MATCH/ );

        my $child_paths = make_dep_tree( $sharedlib, $node_of );

        if ( defined $child_paths ) {
            push @{$new_node}, $child_paths;
        }
    }

    return $new_node;
}


sub make_dep_tree_bfs
{
    my ($file_or_lib) = @_;

    my $root_node = [ $file_or_lib ];
    my (@queue, %was_checked) = $root_node;

    LIBRARY_DEP:
    while (scalar @queue) {
        my $node = shift @queue;
        my $node_name = $node->[0];

        my @needed_libs = eval { readelf_sharedlibs($node_name) };

        next LIBRARY_DEP if ( $was_checked{$node_name} );

        $was_checked{$node_name} = 1;

        # Give up on this path if we can't find a library file...
        if ($@) {
            die $@ if ( $@ !~ /\A$MISSINGLIB_EX/ );
            next LIBRARY_DEP;
        }

        NEW_NODE:
        for my $new_child (@needed_libs) {
            next NEW_NODE if ( $new_child =~ /$IGNORELIB_MATCH/ );
            if ($was_checked{$new_child}) {
                next NEW_NODE if ! $VERBOSE;
                $new_child = "*$new_child";
            }
            my $new_child_node = [ $new_child ];
            push @{$node}, $new_child_node;

            if ( substr $new_child, 0, 1 ne '*' ) {
                push @queue, $new_child_node;
            }
        }
    }

    return $root_node;
}


####
#### SCRIPT START
####

# Get command lines options and print usage if a problem occurs...
my ($show_pkgs, $show_help, $show_man,
    $maxdepth, $use_bfs);

$maxdepth = 5;

GetOptions( 'depth=i',  \$maxdepth,
            'packages', \$show_pkgs,
            'verbose+', \$VERBOSE,
            'bfs',      \$use_bfs,

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

my @output = make_tree_display($deptree, $maxdepth);

# Append the names of packages who own the files to each line if requested...
sub tree_to_packages {
#    my $libname = $_->[0];
#    $libname    =~ s/ \A [*] //xms;
    return ( get_owner_pkg($_->[0]),
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
