package Net::BitTorrent::Types;
{
    use 5.010;
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    %EXPORT_TAGS = (
        infohash => [qw[NBTypes::Infohash NBTypes::Infohash::Packed]],
        tracker  => [
            qw[ NBTypes::Tracker      NBTypes::Tracker::Tier
                NBTypes::Tracker::UDP NBTypes::Tracker::HTTP
                NBTypes::Tracker::HTTP::Event]
        ],
        file    => [qw[NBTypes::Files NBTypes::File::Open::Permission]],
        cache   => [qw[NBTypes::Cache::Packet]],
        client  => [qw[NBTypes::Client::PeerID]],
        dht     => [qw[NBTypes::DHT::NodeID]],
        bencode => [qw[NBTypes::Bencode NBTypes::Bdecode]],
        torrent => [
            qw[NBTypes::Torrent::Status NBTypes::Torrent::Infohash
                NBTypes::Torrent::Bitfield]
        ],
        addr => [qw[NBTypes::Network::Paddr NBTypes::Network::Addr]]
    );
    @EXPORT_OK = sort map { @$_ = sort @$_; @$_ } values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;    # When you want to import everything
    subtype 'NBTypes::Bencode' => as 'Str';
    subtype 'NBTypes::Bdecode' => as 'Ref';
    coerce 'NBTypes::Bencode'  => from 'NBTypes::Bdecode' => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bencode($_);
    };
    coerce 'NBTypes::Bdecode' => from 'NBTypes::Bencode' => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bdecode($_);
    };

    # Nearly the same as NBTypes::DHT::NodeID
    subtype 'NBTypes::Torrent::Infohash' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'Torrent info_hashes are 160-bit integers.'};
    coerce 'NBTypes::Torrent::Infohash' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
    subtype 'NBTypes::Torrent::Bitfield' => as 'Bit::Vector';
    coerce 'NBTypes::Torrent::Bitfield' =>
        from subtype(as 'Str' => where { $_ =~ m[^(?:[10]+)$] }) => via {
        require Bit::Vector;
        Bit::Vector->new_Bin(length($_), scalar reverse $_);
        },
        from subtype(as 'Str' => where { unpack('b*', $_) =~ m[^(?:[10]+)$] }
        ) => via {
        require Bit::Vector;
        my $unpack = scalar reverse unpack 'b*', $_;
        Bit::Vector->new_Bin(length $unpack, $unpack);
        };

    #
    subtype 'NBTypes::Tracker::HTTP' => as
        'Net::BitTorrent::Protocol::BEP03::Tracker::HTTP';
    coerce 'NBTypes::Tracker::HTTP' =>
        from subtype(as 'Str' => where {m[^http://]i}) => via {
        require Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
        return Net::BitTorrent::Protocol::BEP03::Tracker::HTTP->new(
                                                                   url => $_);
        };
    subtype 'NBTypes::Tracker::UDP' => as
        'Net::BitTorrent::Protocol::BEP15::Tracker::UDP';
    coerce 'NBTypes::Tracker::UDP' =>
        from subtype(as 'Str' => where {m[^udp://]i}) => via {
        require Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
        return Net::BitTorrent::Protocol::BEP15::Tracker::UDP->new(url => $_);
        };
    subtype 'NBTypes::Tracker::Tier' => as
        'ArrayRef[NBTypes::Tracker::UDP|NBTypes::Tracker::HTTP]';
    coerce 'NBTypes::Tracker::Tier' => from 'ArrayRef[Str]' => via {
        state $tracker_constraint
            = Moose::Util::TypeConstraints::find_type_constraint(
                              'NBTypes::Tracker::HTTP|NBTypes::Tracker::UDP');
        [map { $tracker_constraint->coerce($_) } @$_];
    };
    enum 'NBTypes::Tracker::HTTP::Event' => qw[started stopped completed];

    #
    enum 'NBTypes::File::Open::Permission' => qw[ro wo rw];
    subtype 'NBTypes::Files' => as 'ArrayRef[Net::BitTorrent::Storage::File]';
    coerce 'NBTypes::Files' => from 'ArrayRef[HashRef]' => via {
        require Net::BitTorrent::Storage::File;
        my ($offset, $index) = (0, 0);
        [map {
             my $obj =
                 Net::BitTorrent::Storage::File->new(
                                           index  => $index++,
                                           length => $_->{'length'},
                                           offset => $offset,
                                           path => [grep {$_} @{$_->{'path'}}]
                 );
             $offset += $_->{'length'};
             $obj
             } @{$_}
        ];
    };
    coerce 'NBTypes::Files' => from 'HashRef' => via {
        require Net::BitTorrent::Storage::File;
        [Net::BitTorrent::Storage::File->new(length => $_->{'length'},
                                             path   => $_->{'path'}
         )
        ];
    };

    #
    subtype 'NBTypes::Cache::Packet' => as 'ArrayRef[Int]' =>
        where { scalar @$_ == 2 };

    #
    subtype 'NBTypes::Client::PeerID' => as 'Str' =>
        where { length $_ == 20 } =>
        message {'PeerID is malformed: length != 20'};

    # Nearly the same as NBTypes::Torrent::Infohash
    subtype 'NBTypes::DHT::NodeID' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'DHT NodeIDs are 160-bit integers.'};
    coerce 'NBTypes::DHT::NodeID' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };

    # IPv6 packed address
    subtype 'NBTypes::Network::Paddr' => as 'Str' =>
        where { length $_ == 16 } =>
        message { sprintf '%s is not 16 bytes', $_ };
    coerce 'NBTypes::Network::Paddr' => from 'Str' => via {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::ip2paddr($_);
    };

    #
    subtype 'NBTypes::Network::Addr' => as 'ArrayRef' =>
        where { $#{$_[0]} == 1 }   => message {'looking for [host, port]'} =>
        where { defined $_[0][0] } => message {'hostname is missing'} =>
        where { defined $_[0][1] } => message {'port is missing'} =>
        where { $_[0][1] =~ m[^\d+$] } => message {'malformed port'};

    #
    no Moose::Util::TypeConstraints;
}
1;

=pod

=head1 NAME

Net::BitTorrent::Types - Moose Types

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id: Types.pm 0aeb1fc 2010-08-02 15:34:35Z sanko@cpan.org $

=cut
