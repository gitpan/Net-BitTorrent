# -*- perl -*-

# /t/500_full_swarm/508_1-to-50.t - Miniature swarm of 1 seed and 50 new peers
# $Id: 508_1-to-50.t 10 2008-04-05 22:06:00Z sanko@cpan.org $

system qq[$^X ./t/data/etc/http_miniswarm.pl 1 50];
