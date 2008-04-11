# -*- perl -*-

# t/200_classes/250_NBSPR/251_new.t
# $Id: 251_new.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../../lib/];

BEGIN { use_ok(q[Net::BitTorrent]) }

