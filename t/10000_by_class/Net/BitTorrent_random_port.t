package t::10000_by_class::Net::BitTorrent_random_port;
{
    use strict;
    use warnings;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../', '../../../', '../../../lib', 'lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent';
    use Test::More;
    use Test::Moose;

    #
    sub port {0}

    #
    __PACKAGE__->runtests() if !caller;
}
1;
