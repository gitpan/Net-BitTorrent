# -*- perl -*-

# t/100_data_parsing/117_uncompact_bad.t - more N::B::Util::uncompact() tests
# $Id: 117_uncompact_bad.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../lib/];

BEGIN{use_ok(q[Net::BitTorrent::Util], qw[:compact])}

