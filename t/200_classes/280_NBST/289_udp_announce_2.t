# -*- perl -*-

# t/200_classes/280_NBST/289_udp_announce_2.t
# $Id: 289_udp_announce_2.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../../lib/];

BEGIN { use_ok(q[Net::BitTorrent]) }

