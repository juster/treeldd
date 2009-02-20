#!/usr/bin/perl

use warnings;
use strict;

use List::MoreUtils  qw(pairwise);
use Getopt::Long     qw(GetOptions);
use Pod::Usage;
use IPC::Cmd         qw(can_run run);
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

Readonly my $READELF_OPTS  => '--dynamic';
Readonly my $READELF_ERROR => qr/ \A readelf: [ ] Error: [ ] (.*) /xms;

Readonly my $READELF_SHARED_LIBS => qr{ Shared[ ]library:
                                        [ ] [[] ([\w.]*) []] }xms;

Readonly my $PACMAN_OPTS   => '-Qo';
Readonly my $PACMAN_ERROR  => qr/ \A error: [ ] (.*?) $ /xms;
Readonly my $PACMAN_OWNER  => qr/ is [ ] owned [ ] by [ ] (.*?) [ ] /xms;

# Ignore libs from packages: glibc gcc-libs crypt
Readonly my $IGNORELIB_LIST =>
    'lib(?:' . ( join '|', ( sort split /\s+/, << 'END_LIST' ) ) . ').so';
BrokenLocale anl c cidn crypt dl m nsl nss_compat nss_dns nss_files
nss_hesiod nss_nis nss_nisplus pthread resolv rt c m crypt X11 z bz2
gcc_s gfortran gomp mudflap mudflapth objc ssp stdc++
END_LIST

Readonly my $IGNORELIB_MATCH => qr/$IGNORELIB_LIST/o;

####
#### GLOBALS
####

my ($PACMAN_PATH, $READELF_PATH);


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

    my ($success, $readelf_output);
    $success = run( command => [ $PACMAN_PATH, $PACMAN_OPTS, $file_path ],
                    buffer => \$readelf_output );

    return '?' if ( !$success );
#        my ($error) = $readelf_output =~ /$PACMAN_ERROR/;
#        die "pacman command failed: $error\n";

    my ($owner_pkg) = $readelf_output =~ /$PACMAN_OWNER/;
    return $owner_pkg || '?';
}

memoize('get_owner_pkg');

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

    my ($success, $readelf_output);

    $success = run( command => [ $READELF_PATH, $READELF_OPTS, $elf_filepath ],
                    buffer  => \$readelf_output );

    if ( !$success ) {
        my ($error) = $readelf_output =~ /$READELF_ERROR/;
        die "readelf command failed: $error\n";
    }

    return grep { $_ !~ /$IGNORELIB_MATCH/ }
        $readelf_output =~ /$READELF_SHARED_LIBS/g;
}
memoize('readelf_sharedlibs');

#---FUNCTION---
# Usage   : my @output_lines = make_tree_display($tree, $maxdepth);
# Params  : $tree - Array reference that represents a tree
#           $maxdepth - How deep in the tree to display, depth 0 is the root
#           $depth_counts - *Internal Use Only* A stack of item counts at each
#                           depth level, used to display vertical lines.
# Returns : Lines suitable to print to screen, output is a textual tree
#           exactly like the 'tree' command-line program.
#--------------

sub print_tree
{
    my ($dep_tree, $max_depth) = @_;
    my ($closure);

    $closure = sub {
        my ($tree, $depth, $depth_counts) = @_;

        return () if ( $depth-- < 0 );

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

        #my @tree_lines = ( "$prefix$node_name\n" );
        print "$prefix$node_name\n";

        # Recurse through the the children nodes...
        my $child_count = $#$tree;
        for my $i ( 1 .. $#$tree ) {
            #push @tree_lines,
            $closure->( $tree->[$i], $depth,
                        [ @$depth_counts,
                          $child_count-- ] );
        }

        #return @tree_lines;
        return;
    };

    return $closure->( $dep_tree, $max_depth, [ ] );
}

sub make_traverser
{
    my ($user_func) = @_;

    my (%we_visited, $closure);

    $closure = sub {
        my $tree_node = shift;
        return if $we_visited{$tree_node};

        $user_func->($tree_node);
        $we_visited{$tree_node} = 1;

        for my $i ( 1 .. $#$tree_node ) {
            $closure->($tree_node->[$i]);
        }
        return;
    };

    return $closure;
}

#---FUNCTION---
# Usage   : my $tree = make_dep_tree( $filepath_or_libname );
# Params  : filepath_or_libname - File or library to become the tree-top.
# Returns : A tree represented as nested array refs.  This tree represents
#           all the dependencies of the program, the deps' dependencies, etc...
#--------------

sub make_dep_tree
{
    # Create a closure in order to keep our hash hidden...
    my (%node_of, $closure);

    $closure = sub {
        my ($file_or_lib) = @_;

        return $node_of{$file_or_lib} if ( exists $node_of{$file_or_lib} );

        my $new_node = [ $file_or_lib ];
        $node_of{$file_or_lib} = $new_node;

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
#            next NEEDED_LIBRARY if ( $VERBOSE < 2 &&
#                                     $sharedlib =~ /$IGNORELIB_MATCH/ );

            my $child_paths = $closure->($sharedlib);

            if ( defined $child_paths ) {
                push @{$new_node}, $child_paths;
            }
        }

        return $new_node;
    };

    return $closure->(@_);
}


####
#### SCRIPT START
####

# Finds readelf and pacman programs...
$PACMAN_PATH  = can_run('pacman')
    or die "error: Cannot find the pacman program, a requirement of $0\n";
$READELF_PATH = can_run('readelf')
    or die "error: Cannot find the readelf program, a requirement of $0\n";

die << 'END_ERROR' if ! IPC::Cmd->can_capture_buffer;
Unknown Internal Error:

The perl IPC::Cmd module reports it cannot capture output from called
programs.  You might need to install the perl IPC::Open3 or IPC::Run
modules for perl on your system.
END_ERROR

# Get command lines options and print usage if a problem occurs...
my ($show_pkgs, $show_help, $show_man,
    $max_depth, $use_bfs, $verbose);

$verbose = 0;

GetOptions( 'depth=i'  => \$max_depth,
            'packages' => \$show_pkgs,
            'verbose+' => \$verbose,

            'help'     => \$show_help,
            'man'      => \$show_man );

my ($elf_file, @target_libs) = @ARGV;

pod2usage({ -verbose => 2 }) if ($show_man);
pod2usage({ -verbose => 1 }) if ($show_help);
pod2usage() if (!$elf_file);
pod2usage("error: $elf_file not found\n") if ( ! -e $elf_file );

$max_depth ||= ( $verbose >= 1 ? 4 : 8 );

# Create our shared library dependency tree...
my $dep_tree = make_dep_tree($elf_file);

# Tack the name of the package who owns the file onto the library name
# if requested with the --packages option.
if ($show_pkgs) {
    my $pkg_tacker = make_traverser( sub {
                                         my $node = shift;
                                         my $owner = get_owner_pkg($node->[0]);
                                         $node->[0] .= " ($owner)";
                                     } );
    $pkg_tacker->($dep_tree);
}

die 'Unknown internal error: $verbose is negative' if ( $verbose < 0 );

# Different levels of verbosity allow for bigger trees...
if ( $verbose < 2 ) {
    my $tree_clipper = make_traverser( do {
        my %has_dup;

        # Create a different clipping action depending on verbosity level...
        my @dispatch =
            (
             # Only keep the first copy of a library that is found
             sub {
                 my ($node, $child_idx) = @_;
                 splice @{$node}, $child_idx, 1;
             },
             # Tag all duplicate copies with '...' & remove children
             sub {
                 my ($node, $child_idx) = @_;
                 my $child_name = $node->[$child_idx][0];
                 $node->[$child_idx] = [ "$child_name ..." ];
             },
            );

        # Search for duplicates and "clip" them...
        my $clip_func = $dispatch[$verbose];
        sub {
            my $node = shift;
            my $i = 1;
            CHILD_NODE:
            while ( $i <= $#$node ) {
                my $child_node = $node->[$i];
                if ($has_dup{$child_node}++) {
                    $clip_func->($node, $i);
                    next CHILD_NODE;
                }
                ++$i;
            }
            return;
        };
    } );

    $tree_clipper->($dep_tree);
}

print_tree( $dep_tree, $max_depth );

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

Justin Davis, C<< jrcd(eighty-three) at gmail dot com >>, aka juster
on L<bbs.archlinux.org>

=head1 COPYRIGHT & LICENSE

Copyright 2009 Justin Davis, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut
