# -*- perl -*-

# t/900_etc/921_perlcritic.t -
# $Id: 921_perlcritic.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

use strict;
use warnings;
use File::Spec;
use Test::More;

if ( not $ENV{TEST_AUTHOR} ) {
    my $msg
        = 'Author test.  Set $ENV{TEST_AUTHOR} to a true value to run.';
    plan( skip_all => $msg );
}

eval { require Test::Perl::Critic; };

if ($@) {
    my $msg = 'Test::Perl::Critic required to criticise code';
    plan( skip_all => $msg );
}

my $rcfile
    = File::Spec->catfile( q[t], q[data], q[etc], q[perlcritic.rc] );
Test::Perl::Critic->import( -profile => $rcfile );
all_critic_ok();
