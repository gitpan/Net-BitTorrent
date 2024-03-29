package Net::BitTorrent::Storage;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../';
    use Net::BitTorrent::Storage::File;
    use Net::BitTorrent::Storage::Cache;
    use File::Spec::Functions qw[rel2abs catdir];
    has 'cache' => (is         => 'ro',
                    isa        => 'Net::BitTorrent::Storage::Cache',
                    init_arg   => undef,
                    lazy_build => 1,
                    builder    => '_build_cache'
    );

    sub _build_cache {
        my $s = shift;
        Net::BitTorrent::Storage::Cache->new(
             storage => $s,
             path =>
                 ['~' . substr($s->torrent->info_hash->to_Hex, 0, 7) . '.dat']
        );
    }
    has 'torrent' => (is       => 'ro',
                      required => 1,
                      isa      => 'Net::BitTorrent::Torrent'
    );
    has 'files' => (is       => 'ro',
                    isa      => 'ArrayRef[Net::BitTorrent::Storage::File]',
                    traits   => ['Array'],
                    writer   => '_set_files',
                    init_arg => undef,
                    handles  => {
                                _count_files => 'count',
                                _add_file    => 'push',
                                _file        => 'get'
                    }
    );

    sub wanted {
        my $s = shift;
        my $b = $s->torrent->have->Shadow;
        for my $file (grep { $_->priority } @{$s->files}) {
            my $min = $file->offset / $s->torrent->piece_length;
            my $max
                = ($file->offset + $file->length) / $s->torrent->piece_length;
            $b->Interval_Fill($min, $max);
        }
        $b->AndNot($b, $s->torrent->have);
        $b;
    }
    sub is_seed { return !shift->wanted->Norm() }
    has 'root' => (    # ??? - Should this be BaseDir/basedir
        is      => 'ro',
        isa     => 'Str',
        writer  => '_set_root',
        default => '.',
        trigger => sub {
            my ($self, $new_root, $old_root) = @_;
            if ($self->_count_files) {

                # XXX - close any files we have open
                #for my $file (@{$self->files}, $self->cache) {
                #    $file->_shift if defined $old_root;
                #    $file->_unshift(rel2abs $new_root);
                #}
            }
        },
        initializer => '_initializer_root'
    );
    around '_set_root' =>
        sub { my ($c, $s, $set) = @_; $c->($s, rel2abs $set) };

    sub _initializer_root {
        my ($s, $c, $set, $attr) = @_;
        $set->(rel2abs $c);
    }

    #
    has 'size' => (is         => 'ro',
                   isa        => 'Int',
                   writer     => '_set_size',
                   lazy_build => 1,
                   builder    => '_build_size'
    );

    sub _build_size {
        my ($self) = @_;
        my $size = 0;
        for my $file (@{$self->files}) { $size += $file->length; }
        return $size;
    }

    sub read {    # Also checks cache
        my ($s, $i, $o, $l) = @_;
        my $data = $s->_read($i, $o, $l) || \'';
        my $x = -1;
        my @cache
            = $s->cache->_map_blocks(sub { $x++; $_->[0] == $i ? $x : () });
        return $data if !@cache;
        warn $data;
        for my $i (@cache) {
            my $where = $s->cache->_get_block_info($i);
            my $d     = $s->cache->get_block($i);
            substr $$data, $where->[1], $where->[2], $d;
        }
        return $data;
    }

    sub _read {
        my ($self, $index, $offset, $length) = @_;
        $offset //= 0;
        $length //=
            int $index == int $self->torrent->piece_count
            ? $self->torrent->size % $self->torrent->piece_length
            : $self->torrent->piece_length;
        my $data       = '';
        my $file_index = 0;
        my $total_offset
            = int(($index * $self->torrent->piece_length) + ($offset || 0));
    SEARCH:
        while ($total_offset > $self->files->[$file_index]->length) {
            $total_offset -= $self->files->[$file_index]->length;
            $file_index++;
            last SEARCH    # XXX - return?
                if not defined $self->files->[$file_index]->length;
        }
    READ: while ((defined $length) && ($length > 0)) {
            my $this_read
                = (($total_offset + $length)
                   >= $self->files->[$file_index]->length)
                ? ($self->files->[$file_index]->length - $total_offset)
                : $length;
            if (!$self->files->[$file_index]->open('ro')) {
                $data .= "\0" x $this_read;
            }
            else {
                my $_data = $self->files->[$file_index]
                    ->read($total_offset, $this_read);
                $data .= $_data if $_data;
            }
            $file_index++;
            $length -= $this_read;
            last READ if not defined $self->files->[$file_index];
            $total_offset = 0;
        }
        return \$data;
    }

    sub write {
        my ($self, $index, $offset, $data) = @_;
        my $file_index = 0;
        my $total_offset
            = int(($index * $self->torrent->piece_length) + ($offset || 0));
    SEARCH:
        while ($total_offset > $self->files->[$file_index]->length) {
            $total_offset -= $self->files->[$file_index]->length;
            $file_index++;
            last SEARCH    # XXX - return?
                if not defined $self->files->[$file_index]->length;
        }
    WRITE: while ((defined $data) && (length $data > 0)) {
            my $this_write
                = (($total_offset + length $data)
                   >= $self->files->[$file_index]->length)
                ? ($self->files->[$file_index]->length - $total_offset)
                : length $data;
            $self->files->[$file_index]->open('wo') or return;
            $self->files->[$file_index]
                ->write($total_offset, substr $data, 0, $this_write, '');
            $file_index++;
            last WRITE if not defined $self->files->[$file_index];
            $total_offset = 0;
        }
        return 1;
    }
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

=for rcs $Id: Storage.pm 15cc184 2010-09-05 19:12:43Z sanko@cpan.org $

=cut
