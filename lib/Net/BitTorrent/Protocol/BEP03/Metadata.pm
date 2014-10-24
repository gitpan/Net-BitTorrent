package Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:bencode :torrent];
    use Net::BitTorrent::Protocol::BEP12::MultiTracker;
    use Net::BitTorrent::Storage;
    use File::Spec::Functions qw[rel2abs];
    use AnyEvent;

    #
    has 'metadata' => (
        isa       => 'NBTypes::Bdecode',
        is        => 'ro',
        writer    => '_set_metadata',
        predicate => '_has_metadata',
        init_arg  => undef,                # cannot set this with new()
        coerce    => 1,
        trigger   => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {                 # parse files and trackers
                $self->tracker->add_tier([$new_value->{'announce'}])
                    if $new_value->{'announce'};
                if (defined $new_value->{'announce-list'}) {
                    $self->tracker->add_tier($_)
                        for @{$new_value->{'announce-list'}};
                }

                #
                my @files;
                if (defined $new_value->{'info'}{'files'})
                {                          # Multi-file .torrent
                    $self->storage->_set_files($new_value->{'info'}{'files'});
                    $self->storage->_set_root($new_value->{'info'}{'name'});
                }
                else {                     # single file torrent; use the name
                    $self->storage->_set_files(
                              [{path   => [$new_value->{'info'}{'name'}],
                                length => $new_value->{'info'}{'length'}
                               }
                              ]
                    );
                }

                #
                if ($_[0]->metadata->{'info'}{'private'}) {
                    require
                        Net::BitTorrent::Protocol::BEP27::Private::Metadata;
                    Net::BitTorrent::Protocol::BEP27::Private::Metadata->meta
                        ->apply($_[0]);
                }
                return 1;
            }
            warn 'Someone changed the metadata!';
            my $info_hash = $self->info_hash;
            $self->_reset_info_hash;
            warn sprintf '%s is now %s', $info_hash->to_Hex,
                $self->info_hash->to_Hex;
        }
    );
    has 'raw_data' => (
        isa        => 'NBTypes::Bencode',
        lazy_build => 1,
        is         => 'ro',
        init_arg   => undef,                # cannot set this with new()
        predicate  => '_has_raw_data',
        writer     => '_set_raw_data',
        coerce     => 1,
        trigger    => sub {
            my ($self, $new_value, $old_value) = @_;
            $self->_set_metadata($new_value) if @_ == 2;

            # XXX - set the current value back to the old value
        }
    );

    #
    my $bencode_constraint;
    has 'info_hash' => (
        is       => 'ro',
        isa      => 'NBTypes::Torrent::Infohash',
        init_arg => undef,                        # cannot set this with new()
        coerce   => 1,                            # Both ways?
        lazy_build => 1,
        builder    => '_build_info_hash',  # returns Torrent::Infohash::Packed
        clearer    => '_reset_info_hash'
    );

    sub _build_info_hash {
        require Digest::SHA;
        my ($self) = @_;
        $bencode_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bencode');
        return if !$self->_has_metadata;
        Digest::SHA::sha1(
                      $bencode_constraint->coerce($self->metadata->{'info'}));
    }
    has 'piece_count' => (is         => 'ro',
                          isa        => 'Int',
                          lazy_build => 1,
                          builder    => '_build_piece_count',
                          init_arg   => undef
    );
    sub _build_piece_count { return length(shift->pieces) / 20 }
    has 'tracker' => (is  => 'ro',
                      isa => 'Net::BitTorrent::Protocol::BEP12::MultiTracker',
                      predicate => 'has_tracker',
                      builder   => '_build_tracker'
    );

    sub _build_tracker {
        require Net::BitTorrent::Protocol::BEP12::MultiTracker;
        Net::BitTorrent::Protocol::BEP12::MultiTracker->new(
                                                           metadata => shift);
    }

    # Quick accessors
    sub piece_length { shift->metadata->{'info'}{'piece length'} }
    sub pieces       { shift->metadata->{'info'}{'pieces'} }
    sub private {0}    # overridden by BEP27::Private::Metadata
}
1;

=pod

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

=for rcs $Id: Metadata.pm 02a9026 2010-08-02 19:30:47Z sanko@cpan.org $

=cut
