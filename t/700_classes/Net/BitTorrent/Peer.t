#!/usr/bin/perl -w
use strict;
use warnings;
use Test::More;
use Module::Build;
use Socket qw[AF_INET SOCK_STREAM INADDR_LOOPBACK SOL_SOCKET
    sockaddr_in unpack_sockaddr_in inet_ntoa];
use File::Temp qw[tempdir];
use Scalar::Util qw[/weak/];
use Time::HiRes qw[];
use lib q[../../../../lib];
use Net::BitTorrent;
use Net::BitTorrent::Torrent;
use Net::BitTorrent::Peer;
use Net::BitTorrent::Protocol qw[:all];
$|++;
my $test_builder       = Test::More->builder;
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
chdir q[../../../../] if not -f $simple_dot_torrent;
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
my $threads         = $build->notes(q[threads]);
my ($flux_capacitor, %peers) = (0, ());
plan tests => 91;

BEGIN {
    *CORE::GLOBAL::time
        = sub () { return CORE::time + ($flux_capacitor * 60); };
}
SKIP: {
    skip(
        q[Due to system configuration, tcp-related tests have been disabled.  ...which makes N::B pretty useless.],
        ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) if !$okay_tcp;

#skip(
#    q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable],
#         ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
#) if !$release_testing;
    my %client;

    END {
        for my $client (values %client) {
            for my $torrent (values %{$client->torrents}) {
                for my $file (@{$torrent->files}) {
                    if ($file->mode) { $file->_close(); }
                }
            }
        }
    }
    {    # Client A
        $client{q[A]} = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
        isa_ok($client{q[A]},
               q[Net::BitTorrent],
               sprintf(q[Client  A%s],
                       $client{q[A]}
                       ? q[ (pid:]
                           . $client{q[A]}->peerid
                           . q[, tcp:]
                           . $client{q[A]}->_tcp_port . q[)]
                       : q[])
        );

       #isa_ok($client{q[A]}->add_torrent({Path => $simple_dot_torrent,
       #                                   BaseDir =>
       #                                       tempdir(q[~NBSF_test_XXXXXXXX],
       #                                               CLEANUP => 1,
       #                                               TMPDIR  => 1
       #                                       )
       #                                  }
       #       ),
       #       q[Net::BitTorrent::Torrent],
       #       q[Torrent A]
       #);
        my ($_address,    # defined in peer_connect
            $_peer,       # defined in ip_filter
            $_read        # defined (and updated if needed) by peer_read
        );
        ok( $client{q[A]}->on_event(
                q[ip_filter],
                sub {
                    my ($self, $params) = @_;
                    is($self, $client{q[A]},
                        q[Object handed to callback matches what we expected [ip_filter]]
                    );
                    like($_address = delete($params->{q[Address]}),
                         qr[^(.+:\d+)$],
                         q[Params contain a properly formated 'Address' value [ip_filter]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [ip_filter]]
                    );
                }
            ),
            q[Set customized 'ip_filter' callback for Client A]
        );
        ok( $client{q[A]}->on_event(
                q[peer_write],
                sub {
                    my ($self, $params) = @_;
                    my ($explain) = explain $params;
                    die(q[We've sent a packet to a peer for reasons beyond me: ]
                            . $explain);
                }
            ),
            q[Set customized 'peer_write' callback for Client A]
        );
        ok( $client{q[A]}->on_event(
                q[peer_read],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[A]},
                        q[Object handed to callback matches what we expected [peer_read]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [peer_read]]
                    );
                    like($_read += delete($params->{q[Length]}),
                         qr[^\d+$],
                         q[Params contains the 'Length' of data we read from this peer [peer_read]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [peer_read]]
                    );
                    is($_peer->host . q[:] . $_peer->port,
                        $_address, q[Resolved host is as expected]);
                    is($_peer->am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, undef,
                        q[Internal status: We have not parsed their peerid)]);
                    is($_peer->reserved_bytes, undef,
                        q[Internal status: We have not parsed their reserved bytes)]
                    );
                    is($_peer->torrent, undef,
                        q[Internal status: We have not parsed their infohash)]
                    );
                    is($_peer->bitfield, undef,
                        q[Internal status: We have not parsed their bitfield)]
                    );
                    isa_ok($_peer->_socket, q[GLOB],
                           q[Internal status: Peer has a socket (duh))]);
                }
            ),
            q[Set customized 'peer_read' callback for Client A]
        );
        ok( $client{q[A]}->on_event(
                q[peer_connect],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[A]},
                        q[Object handed to callback matches what we expected [peer_connect]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [peer_connect]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [peer_connect]]
                    );
                    is($_peer->host . q[:] . $_peer->port,
                        $_address, q[Resolved host is as expected]);
                    is($_peer->am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, undef,
                        q[Internal status: We have not recieved their peerid)]
                    );
                    is($_peer->reserved_bytes, undef,
                        q[Internal status: We have not recieved their reserved bytes)]
                    );
                    is($_peer->torrent, undef,
                        q[Internal status: We have not recieved their infohash)]
                    );
                    is($_peer->bitfield, undef,
                        q[Internal status: We have not recieved their bitfield)]
                    );
                    isa_ok($_peer->_socket, q[GLOB],
                           q[Internal status: Peer has a socket (duh))]);
                }
            ),
            q[Set customized 'peer_connect' callback for Client A]
        );
        ok( $client{q[A]}->on_event(
                q[peer_disconnect],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[A]},
                        q[Object handed to callback matches what we expected [peer_disconnect]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [peer_disconnect]]
                    );
                    is(delete($params->{q[Reason]}), -11,
                        q[Params contain a 'Reason' (-11: We aren't serving this torrent) [peer_disconnect]]
                    );
                    is_deeply(delete $params->{q[Advanced]},
                              {Infohash => unpack q[H40], (q[A] x 20)},
                              q[This particular disconnection comes with some 'Advanced' parameters [peer_disconnect]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [peer_disconnect]]
                    );
                TODO: {
                        local $TODO = q[I may cache these in the future];
                        is( ($_peer->_host || q[]) . q[:]
                                . ($_peer->_port || q[]),
                            $_address,
                            q[Resolved host is as expected]
                        );
                    }
                    is($_peer->_am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->_peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->_am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->_peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->_incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->_source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, q[B] x 20,
                        q[Internal status: We have recieved their peerid)]);
                    is( $_peer->_reserved_bytes,
                        qq[\0] x 8,
                        q[Internal status: We have recieved their reserved bytes)]
                    );
                    is($_peer->_torrent, undef,
                        q[Internal status: We have not recieved their infohash)]
                    );
                    is($_peer->_bitfield, undef,
                        q[Internal status: We have not recieved their bitfield)]
                    );
                    is($_peer->_socket, undef,
                        q[Internal status: Peer no longer has a socket]);
                }
            ),
            q[Set customized 'peer_disconnect' callback for Client A]
        );
        ok( $client{q[A]}->on_event(
                q[outgoing_packet],
                sub {
                    my ($self, $params) = @_;
                    my ($explain) = explain $params;
                    die(q[We've sent a packet to a peer for reasons beyond me: ]
                            . $explain);
                }
            ),
            q[Set customized 'outgoing_packet' callback for Client A]
        );
        ok( $client{q[A]}->on_event(
                q[incoming_packet],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[A]},
                        q[Object handed to callback matches what we expected [incoming_packet]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [incoming_packet]]
                    );
                    is_deeply(delete $params->{q[Type]},
                              Net::BitTorrent::Peer::HANDSHAKE(),
                              q[We are (only) expecting a handshake from this peer [incoming_packet]]
                    );
                    is_deeply(delete $params->{q[Payload]},
                              {Infohash => q[A] x 20,
                               PeerID   => q[B] x 20,
                               Reserved => qq[\0] x 8,
                              },
                              q[Payload for this handshake is what we expected it to be [incoming_packet]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [incoming_packet]]
                    );
                    is($_peer->_host . q[:] . $_peer->_port,
                        $_address, q[Resolved host is as expected]);
                    is($_peer->_am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->_peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->_am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->_peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->_incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->_source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, q[B] x 20,
                        q[Internal status: We have recieved their peerid)]);
                    is( $_peer->_reserved_bytes,
                        qq[\0] x 8,
                        q[Internal status: We have recieved their reserved bytes)]
                    );
                    is($_peer->_torrent, undef,
                        q[Internal status: We have recieved their infohash but we aren't serving this torrent)]
                    );
                    is($_peer->_bitfield, undef,
                        q[Internal status: We have not recieved their bitfield (and never will))]
                    );
                    isa_ok($_peer->_socket,
                           q[GLOB],
                           q[Internal status: Peer (still) has a socket (for now))]
                    );
                }
            ),
            q[Set customized 'peer_connect' callback for Client A]
        );
        my $newsock_A = newsock($client{q[A]});
        $client{q[A]}->do_one_loop(1);
        is( syswrite($newsock_A,
                     build_handshake(chr(0) x 8, q[A] x 20, q[B] x 20)
            ),
            68,
            q[Send handshake to Client A]
        );
        for my $iteration (1 .. 10) {
            $client{q[A]}->do_one_loop(1);
            last if $_read == 68;
        }
        is($_read, 68, q[We read the entire handshake and nothing more]);
    }
    is($test_builder->{q[Curr_Test]},
        44, q[*** The test suite is on track after Client A]);
##############################################################################
    {    # Client B
        $client{q[B]} = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
        isa_ok($client{q[B]},
               q[Net::BitTorrent],
               sprintf(q[Client  A%s],
                       $client{q[B]}
                       ? q[ (pid:]
                           . $client{q[B]}->peerid
                           . q[, tcp:]
                           . $client{q[B]}->_tcp_port . q[)]
                       : q[])
        );
        isa_ok($client{q[B]}->add_torrent({Path => $simple_dot_torrent,
                                           BaseDir =>
                                               tempdir(q[~NBSF_test_XXXXXXXX],
                                                       CLEANUP => 1,
                                                       TMPDIR  => 1
                                               )
                                          }
               ),
               q[Net::BitTorrent::Torrent],
               q[Torrent B]
        );
        my ($_address,    # defined in peer_connect
            $_peer,       # defined in ip_filter
            $_read        # defined (and updated if needed) by peer_read
        );
        ok( $client{q[B]}->on_event(
                q[ip_filter],
                sub {
                    my ($self, $params) = @_;
                    is($self, $client{q[B]},
                        q[Object handed to callback matches what we expected [ip_filter]]
                    );
                    like($_address = delete($params->{q[Address]}),
                         qr[^(.+:\d+)$],
                         q[Params contain a properly formated 'Address' value [ip_filter]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [ip_filter]]
                    );
                }
            ),
            q[Set customized 'ip_filter' callback for Client B]
        );
        ok( $client{q[B]}->on_event(
                q[peer_write],
                sub {
                    my ($self, $params) = @_;
                    my ($explain) = explain $params;
                    die(q[We've sent a packet to a peer for reasons beyond me: ]
                            . $explain);
                }
            ),
            q[Set customized 'peer_write' callback for Client B]
        );
        ok( $client{q[B]}->on_event(
                q[peer_read],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[B]},
                        q[Object handed to callback matches what we expected [peer_read]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [peer_read]]
                    );
                    like($_read += delete($params->{q[Length]}),
                         qr[^\d+$],
                         q[Params contains the 'Length' of data we read from this peer [peer_read]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [peer_read]]
                    );
                    is($_peer->host . q[:] . $_peer->port,
                        $_address, q[Resolved host is as expected]);
                    is($_peer->am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, undef,
                        q[Internal status: We have not parsed their peerid)]);
                    is($_peer->reserved_bytes, undef,
                        q[Internal status: We have not parsed their reserved bytes)]
                    );
                    is($_peer->torrent, undef,
                        q[Internal status: We have not parsed their infohash)]
                    );
                    is($_peer->bitfield, undef,
                        q[Internal status: We have not parsed their bitfield)]
                    );
                    isa_ok($_peer->_socket, q[GLOB],
                           q[Internal status: Peer has a socket (duh))]);
                }
            ),
            q[Set customized 'peer_read' callback for Client B]
        );
        ok( $client{q[B]}->on_event(
                q[peer_connect],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[B]},
                        q[Object handed to callback matches what we expected [peer_connect]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [peer_connect]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [peer_connect]]
                    );
                    is($_peer->host . q[:] . $_peer->port,
                        $_address, q[Resolved host is as expected]);
                    is($_peer->am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, undef,
                        q[Internal status: We have not recieved their peerid)]
                    );
                    is($_peer->reserved_bytes, undef,
                        q[Internal status: We have not recieved their reserved bytes)]
                    );
                    is($_peer->torrent, undef,
                        q[Internal status: We have not recieved their infohash)]
                    );
                    is($_peer->bitfield, undef,
                        q[Internal status: We have not recieved their bitfield)]
                    );
                    isa_ok($_peer->_socket, q[GLOB],
                           q[Internal status: Peer has a socket (duh))]);
                }
            ),
            q[Set customized 'peer_connect' callback for Client B]
        );
        ok( $client{q[B]}->on_event(
                q[peer_disconnect],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[B]},
                        q[Object handed to callback matches what we expected [peer_disconnect]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [peer_disconnect]]
                    );
                    is(delete($params->{q[Reason]}), -13,
                        q[Params contain a 'Reason' (-13: We aren't serving this torrent) [peer_disconnect]]
                    );
                    is_deeply(delete $params->{q[Advanced]},
                              {Infohash => unpack q[H40], (q[A] x 20)},
                              q[This particular disconnection comes with some 'Advanced' parameters [peer_disconnect]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [peer_disconnect]]
                    );
                TODO: {
                        local $TODO = q[I may cache these in the future];
                        is( ($_peer->_host || q[]) . q[:]
                                . ($_peer->_port || q[]),
                            $_address,
                            q[Resolved host is as expected]
                        );
                    }
                    is($_peer->_am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->_peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->_am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->_peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->_incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->_source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, q[B] x 20,
                        q[Internal status: We have recieved their peerid)]);
                    is( $_peer->_reserved_bytes,
                        qq[\0] x 8,
                        q[Internal status: We have recieved their reserved bytes)]
                    );
                    is($_peer->_torrent, undef,
                        q[Internal status: We have not recieved their infohash)]
                    );
                    is($_peer->_bitfield, undef,
                        q[Internal status: We have not recieved their bitfield)]
                    );
                    is($_peer->_socket, undef,
                        q[Internal status: Peer no longer has a socket]);
                }
            ),
            q[Set customized 'peer_disconnect' callback for Client B]
        );
        ok( $client{q[B]}->on_event(
                q[outgoing_packet],
                sub {
                    my ($self, $params) = @_;
                    my ($explain) = explain $params;

                 #die(q[We've sent a packet to a peer for reasons beyond me: ]
                 #        . $explain);
                }
            ),
            q[Set customized 'outgoing_packet' callback for Client B]
        );
        ok( $client{q[B]}->on_event(
                q[incoming_packet],
                sub {
                    my ($self, $params) = @_;
                    is( $self,
                        $client{q[B]},
                        q[Object handed to callback matches what we expected [incoming_packet]],
                    );
                    isa_ok($_peer = delete($params->{q[Peer]}),
                           q[Net::BitTorrent::Peer],
                           q[Params contain a blessed N::B::Peer object in 'Peer' [incoming_packet]]
                    );
                    is_deeply(delete $params->{q[Type]},
                              Net::BitTorrent::Peer::HANDSHAKE(),
                              q[We are (only) expecting a handshake from this peer [incoming_packet]]
                    );
                    is_deeply(delete $params->{q[Payload]},
                              {Infohash =>
                                   pack(q[H40], (keys %{$self->torrents})[0]),
                               PeerID   => q[B] x 20,
                               Reserved => qq[\0] x 8,
                              },
                              q[Payload for this handshake is what we expected it to be [incoming_packet]]
                    );
                    is_deeply(
                        $params,
                        {},
                        q[Params contain no other data as exptexed [incoming_packet]]
                    );
                    is($_peer->_host . q[:] . $_peer->_port,
                        $_address, q[Resolved host is as expected]);
                    is($_peer->_am_choking, 1,
                        q[Initial status: Peer is choked]);
                    is($_peer->_peer_choking, 1,
                        q[Initial status: Peer is choking us]);
                    is($_peer->_am_interested, 0,
                        q[Initial status: Peer is not interesting]);
                    is($_peer->_peer_interested, 0,
                        q[Initial status: Peer is not interested]);
                    is($_peer->_incoming, 1,
                        q[Internal status: Peer initiated this connection (_incoming())]
                    );
                    is($_peer->_source, q[Incoming],
                        q[Internal status: Peer initiated this connection (_source())]
                    );
                    is($_peer->peerid, q[B] x 20,
                        q[Internal status: We have recieved their peerid)]);
                    is( $_peer->_reserved_bytes,
                        qq[\0] x 8,
                        q[Internal status: We have recieved their reserved bytes)]
                    );
                    is($_peer->_torrent, undef,
                        q[Internal status: We have recieved their infohash but we aren't serving this torrent)]
                    );
                    is($_peer->_bitfield, undef,
                        q[Internal status: We have not recieved their bitfield (and never will))]
                    );
                    isa_ok($_peer->_socket,
                           q[GLOB],
                           q[Internal status: Peer (still) has a socket (for now))]
                    );
                }
            ),
            q[Set customized 'peer_connect' callback for Client B]
        );
        my $newsock_A = newsock($client{q[B]});
        $client{q[B]}->do_one_loop(1);
        is( syswrite($newsock_A,
                     build_handshake(
                                   chr(0) x 8,
                                   pack(q[H40],
                                        (keys %{$client{q[B]}->torrents})[0]),
                                   q[B] x 20
                     )
            ),
            68,
            q[Send handshake to Client B]
        );
        for my $iteration (1 .. 10) {
            $client{q[B]}->do_one_loop(1);
            last if $_read == 68;
        }
        is($_read, 68, q[We read the entire handshake and nothing more]);
    }
    is($test_builder->{q[Curr_Test]},
        90, q[*** The test suite is on track after Client B]);
}

sub newsock {
    my ($server) = @_;
    my ($port, $packed_ip) = unpack_sockaddr_in(getsockname($server->_tcp));
    my $outgoing;
    socket($outgoing, AF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
        ? do {
        diag(sprintf q[Creating new sockpair to connect to %s:%d (%s)],
             inet_ntoa($packed_ip), $port, $server->peerid);
        connect($outgoing, getsockname($server->_tcp));
        }
        : die(
            sprintf q[Failed to create new sockpair to connect to %s:%d (%s)],
            inet_ntoa($packed_ip), $port, $server->peerid);
    return $outgoing;
}
__END__
Copyright (C) 2008-2009 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it
under the terms of The Artistic License 2.0.  See the LICENSE file
included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered by
the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

$Id: Peer.t 91d4c6b 2009-08-31 03:58:10Z sanko@cpan.org $
