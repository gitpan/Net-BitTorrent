package t::10000_by_class::Net::BitTorrent::Network::IPFilter::Range;
{
    use strict;
    use warnings;
    use Test::Most;
    use parent 'Test::Class';
    use lib '../../../../../../lib', 'lib';

    #
    sub class {'Net::BitTorrent::Network::IPFilter::Range'}

    sub new_args {
        lower            => '127.0.0.1',
            upper        => '127.0.0.5',
            description  => 'Testing',
            access_level => 559;
    }

    sub deeply {
        my %d = new_args;
        require Net::BitTorrent::Network::Utility;
        map { $_ = Net::BitTorrent::Network::Utility::ip2paddr($_) }
            $d{'lower'}, $d{'upper'};
        bless \%d, class;
    }

    #
    sub startup : Tests( startup => 3 ) {
        my $self = shift;
        use_ok $self->class;
        can_ok $self->class, 'new';
        $self->{'range'} = new_ok $self->class, [$self->new_args];
    }

    sub setup : Test( setup ) {
        my $self = shift;
    }

    sub test_deeply : Test( 1 ) {
        my $s = shift;
        my $r = $s->{'range'};
        is_deeply $r, $s->deeply, 'internals check out';
    }

    sub attributes : Test( 6 ) {
        my $s = shift;
        my $r = $s->{'range'};
        require Net::BitTorrent::Network::Utility;
        is $r->$_(),
            Net::BitTorrent::Network::Utility::ip2paddr({$s->new_args}->{$_}),
            $_ . ' limit checks out'
            for qw[lower upper];
        is $r->$_(), {$s->new_args}->{$_}, $_ . ' checks out'
            for qw[access_level description];
        is $r->lower_as_string(), {$s->new_args}->{'lower'},
            'lower_as_string checks out';
        is $r->upper_as_string(), {$s->new_args}->{'upper'},
            'upper_as_string checks out';
    }

    sub in_range : Test( 2 ) {
        my $s = shift;
        my $r = $s->{'range'};
        ok $r->in_range('127.0.0.2'), '127.0.0.2 is in range';
        ok !$r->in_range('127.0.0.9'), '127.0.0.9 is beyond range';
    }

    sub z_access_level : Test( 4 ) {
        my $s       = shift;
        my $r       = $s->{'range'};
        my $initial = $r->access_level;
        $r->increase_access_level();
        is $r->access_level, $initial + 1,
            '->increase_access_level( ) increased the access_level by 1';
        $r->increase_access_level(2);
        is $r->access_level, $initial + 3,
            '->increase_access_level( 2 ) increased the access_level by 2';
        $r->decrease_access_level(3);
        is $r->access_level, $initial,
            '->decrease_access_level( 3 ) returns access_level to the original value';
        $r->set_access_level(655);
        is $r->access_level, 655,
            '->set_access_level( 655 ) sets access_level to 655';
    }

    sub _as_string : Test( 1 ) {
        my $s = shift;
        my $r = $s->{'range'};
        is $r->_as_string, '127.0.0.1, 127.0.0.5, 559, Testing',
            '->_as_string is as okay';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
