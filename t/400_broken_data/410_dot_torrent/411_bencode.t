# -*- perl -*-

# t/400_broken_data/410_dot_torrent/411_bencode.t
# $Id: 411_bencode.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../../lib/];

BEGIN { use_ok(q[Net::BitTorrent]) }

