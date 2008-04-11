# -*- perl -*-

# t/200_classes/280_NBST/288_udp_scrape.t
# $Id: 288_udp_scrape.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../../lib/];

BEGIN { use_ok(q[Net::BitTorrent]) }

