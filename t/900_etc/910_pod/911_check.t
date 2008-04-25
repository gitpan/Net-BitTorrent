# -*- perl -*-

# t/900_etc/910_pod/911_check.t -
# $Id: 911_check.t 18 2008-04-25 01:14:52Z sanko@cpan.org $

use strict;
use warnings;

use Test::More;

if ( not $ENV{TEST_AUTHOR} ) {
    plan( skip_all =>
        q[Author test.  Set $ENV{TEST_AUTHOR} to a true value to run.]
    );
}

eval q[use Test::Pod 1.00];

plan skip_all => q[Test::Pod 1.00 required for testing POD] if $@;
all_pod_files_ok();
