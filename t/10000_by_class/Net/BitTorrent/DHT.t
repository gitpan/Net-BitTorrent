package t::10000_by_class::Net::BitTorrent::DHT;
{
    use strict;
    use warnings;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use Test::More;
    use parent 'Test::Class';
    use lib '../../../../lib', 'lib';
    use 5.010.000;
    use Test::Moose;
    use Test::More;
    use AnyEvent::Impl::Perl;   # Timing is different than with EV. Go figure.
    use AnyEvent;

    #
    sub class {'Net::BitTorrent::DHT'}

    sub new_args {
        my $t = shift;
        require Net::BitTorrent;
        [                       #port              => [1337 .. 1339, 0],
           on_listen_failure => sub {
               my ($s, $a) = @_;
               note $a->{'message'};
               $t->{'cv'}->end if $a->{'protocol'} =~ m[udp];
           },
           on_listen_success =>
               sub { my ($s, $a) = @_; note $a->{'message'}; }
        ];
    }

    #
    sub startup : Tests(startup => no_plan) {
        my $self = shift;
        use_ok $self->class;
        can_ok $self->class, 'new';
        explain $self->new_args;
        $self->{'dht'}
            = new_ok('Net::BitTorrent', $self->new_args, 'decoy NB client')
            ->dht;
        $self->{'dht'}->add_node($_)
            for ['router.utorrent.com', 6881],
            ['router.bittorrent.com', 6881];
    }

    sub setup : Test(setup) {
        my $self = shift;
    }

    sub creation : Test {
        my $self = shift;
        isa_ok($self->{dht}, $self->class)
            or $self->FAIL_ALL($self->class . '->new failed');
    }

    sub nodeid : Test {
        my $pig = shift->{dht};
        ok($pig->nodeid, 'nodeid is defined');
    }

    sub check_role : Test( 1 ) {
        my $self = shift;
        ok $self->{'dht'}->has_client,
            '... standard dht nodes have a parent client';
    }

    sub _000_init : Test( startup ) {
        my $s = shift;
        note 'Adding condvar for later use...';
        $s->{'cv'} = AE::cv();
        $s->{'cv'}->begin(sub { $s->{'cv'}->send });
        note '...which will timeout in 2m.';
        $s->{'to'} = AE::timer(
            60 * 2,
            0,
            sub {
                note sprintf 'Timeout waiting for %s!', join ', ',
                    keys %{$s->{'todo'}};
                $s->{'cv'}->send;
            }
        );
    }

    sub wait : Test( shutdown => no_plan ) {
        my $s = shift;
        $s->{'cv'}->end;
        $s->{'cv'}->recv;
    }

    sub quest_find_node : Test( no_plan ) {
        my $s = shift;
        $s->{'todo'}{'find_node'}++;
        $s->{'cv'}->begin;
        my $l = join '', map { [0 .. 9, 'a' .. 'f']->[int rand(16)] } 1 .. 40;
        note 'Seeking nodes near ' . $l;
        $s->{'quest'}{'find_node'} = $s->{'dht'}->find_node(
            $l,
            sub {
                my ($tar, $nd, $pr) = @_;
                subtest 'find_node callback' => sub {
                    plan tests => 3;
                    isa_ok($tar, 'Bit::Vector',
                           'Target isa a Bit::Vector object');
                    isa_ok($nd,
                           'Net::BitTorrent::Protocol::BEP05::Node',
                           'Node is a ...::Node');
                    is ref $pr, 'ARRAY',
                        'List of close nodes is... a list... of addrs?';
                    note sprintf
                        'We found %d nodes near %s from [\'%s\', %d] via DHT',
                        scalar(@$pr),
                        $tar->to_Hex, $nd->host, $nd->port;
                    note join ', ', map { sprintf '[\'%s\', %d]', @$_ } @$pr;
                    delete $s->{'todo'}{'find_node'};
                };
                state $done = 0;
                $s->{'cv'}->end if !$done++;
            }
        );
        ok($s->{'quest'}{'find_node'});
        is ref $s->{'quest'}{'find_node'}, 'ARRAY',
            'find_node quest is an array reference';
    }

    sub quest_announce_peer : Test( no_plan ) {
        my $s = shift;
        $s->{'todo'}{'announce_peer'}++;
        $s->{'cv'}->begin;
        $s->{'ih'} = '6d0f88e9646c0f3a01bc35d0b0845db3247e6260';
        $s->{'po'} = $s->{'dht'}->port;
        note sprintf 'Pretending we are serving %s on port %d', $s->{'ih'},
            $s->{'po'};
        $s->{'quest'}{'announce_peer'} = $s->{'dht'}->announce_peer(
            $s->{'ih'},
            $s->{'po'},
            sub {
                my ($infohash, $node, $port) = @_;
                subtest 'announce_peer_callback' => sub {
                    plan tests => 3;
                    isa_ok($infohash, 'Bit::Vector',
                           'Infohash isa a Bit::Vector object');
                    isa_ok($node,
                           'Net::BitTorrent::Protocol::BEP05::Node',
                           'Node is a ...::Node');
                    ok $port =~ m[^\d+$], 'Port is... a number';
                    note sprintf
                        'Announced %s on port %d with [\'%s\', %d] (%s)',
                        $infohash->to_Hex, $port, $node->host, $node->port,
                        $node->nodeid->to_Hex;
                    delete $s->{'todo'}{'announce_peer'};
                };
                state $done = 0;
                $s->{'cv'}->end if !$done++;
            }
        );
        ok($s->{'quest'}{'announce_peer'});
        is ref $s->{'quest'}{'announce_peer'}, 'ARRAY',
            'announce_peer quest is an array reference';
    }

    sub quest_get_peers : Test( no_plan ) {
        my $s = shift;
        $s->{'todo'}{'get_peers'}++;
        $s->{'cv'}->begin;
        note 'Seeking peers with ', $s->{'ih'};
        $s->{'quest'}{'get_peers'} = $s->{'dht'}->get_peers(
            $s->{'ih'},
            sub {
                my ($ih, $nd, $pr) = @_;
                subtest 'get_peers callback' => sub {
                    plan tests => 3;
                    isa_ok($ih, 'Bit::Vector',
                           'Infohash isa a Bit::Vector object');
                    isa_ok($nd,
                           'Net::BitTorrent::Protocol::BEP05::Node',
                           'Node is a ...::Node');
                    is ref $pr, 'ARRAY',
                        'List of peers is... a list... of peers?';
                    note sprintf
                        'We found %d peers for %s from [\'%s\', %d] via DHT',
                        scalar(@$pr),
                        $ih->to_Hex, $nd->host, $nd->port;
                    note join ', ', map { sprintf '[\'%s\', %d]', @$_ } @$pr;
                    delete $s->{'todo'}{'get_peers'};
                };
                state $done = 0;
                $s->{'cv'}->end if !$done++;
            }
        );
        ok($s->{'quest'}{'get_peers'});
        is ref $s->{'quest'}{'get_peers'}, 'ARRAY',
            'get_peers quest is an array reference';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
