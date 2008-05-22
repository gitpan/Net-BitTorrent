# -*- perl -*-

# t/900_etc/910_pod/912_coverge.t -
# $Id: 912_coverage.t 20 2008-05-22 23:09:05Z sanko@cpan.org $

use strict;
use warnings;

use Test::More;

if ( not $ENV{RELEASE_TESTING} ) {
    plan( skip_all =>
        q[Release test.  Set $ENV{RELEASE_TESTING} to a true value to run.]
    );
}

eval q[use Test::Pod::Coverage;] and 1;

if (    $@
     or ( not defined $Test::Pod::Coverage::VERSION )
     or ( $Test::Pod::Coverage::VERSION < 1.08 ) )
{
    plan( skip_all =>
         q[Test::Pod::Coverage 1.08 required for testing POD coverage]
    );
}
elsif (    ( not defined $Pod::Coverage::VERSION )
        or ( $Pod::Coverage::VERSION < 0.19 ) )
{
    plan( skip_all =>
          q[Pod::Coverage 0.19 required for testing POD coverage] );
}

all_pod_coverage_ok();
