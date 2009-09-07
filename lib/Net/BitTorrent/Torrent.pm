#!/usr/bin/perl -w
package Net::BitTorrent::Torrent;
{
    use strict;
    use warnings;
    use Digest::SHA qw[sha1_hex];
    use Carp qw[carp carp];
    use Cwd qw[cwd];
    use File::Spec::Functions qw[rel2abs catfile];
    use Scalar::Util qw[blessed weaken refaddr];
    use List::Util qw[sum shuffle max min];
    use Fcntl qw[/O_/ /SEEK/ :flock];
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    @EXPORT_OK = qw[
        STARTED CHECKING START_AFTER_CHECK CHECKED
        ERROR   PAUSED   LOADED            QUEUED
    ];
    %EXPORT_TAGS = (status => [@EXPORT_OK], all => [@EXPORT_OK]);
    use lib q[../../../lib];
    use Net::BitTorrent::Util qw[:bencode :compact];
    use Net::BitTorrent::Peer qw[];
    use Net::BitTorrent::Torrent::File;
    use Net::BitTorrent::Torrent::Tracker;
    our $VERSION_BASE = 50; our $UNSTABLE_RELEASE = 12; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), ($VERSION_BASE / 1000), $UNSTABLE_RELEASE);
    my %REGISTRY = ();
    my @CONTENTS = \my (
        %_client,  %path,            %_basedir,
        %size,     %files,           %trackers,
        %infohash, %uploaded,        %downloaded,
        %bitfield, %_working_pieces, %_block_length,
        %status,   %error,
        %_event,   %resume_path,     %_nodes,

        # Alpha code
        %metadata, %_metadata_size, %_metadata_requests,
        %_metadata_raw
    );
    my %_char2bits; # For decoding 32bit Magnet links. Cached only when needed
    sub STARTED           {1}
    sub CHECKING          {2}
    sub START_AFTER_CHECK {4}
    sub CHECKED           {8}
    sub ERROR             {16}
    sub PAUSED            {32}
    sub LOADED            {64}
    sub QUEUED            {128}
    sub HAVE_METADATA     {256}

    sub new {
        my ($class, $args) = @_;
        my $self = bless \$class, $class;
        if ((!$args) || (ref($args) ne q[HASH])) {
            carp q[Net::BitTorrent::Torrent->new({ }) requires ]
                . q[parameters to be passed as a hashref];
            return;
        }

        #
        if ($args->{q[Status]} and $args->{q[Status]} !~ m[^\d+$]) {
            carp q[Net::BitTorrent::Torrent->new({ }) requires an ]
                . q[integer 'Status' parameter.  Falling back to defaults.];
            delete $args->{q[Status]};
        }
        $args->{q[BaseDir]} = rel2abs(
                  defined($args->{q[BaseDir]}) ? $args->{q[BaseDir]} : cwd());
        $_basedir{refaddr $self} = $args->{q[BaseDir]};
        $args->{q[Status]} ||= 0;
        $args->{q[Status]} ^= CHECKING if $args->{q[Status]} & CHECKING;
        $args->{q[Status]} ^= CHECKED  if $args->{q[Status]} & CHECKED;
        $args->{q[Status]} ^= ERROR    if $args->{q[Status]} & ERROR;
        $args->{q[Status]} ^= LOADED   if $args->{q[Status]} & LOADED;
        ${$status{refaddr $self}} = $args->{q[Status]};
        $trackers{refaddr $self} = [];

        #
        if ($args->{q[Magnet]}) {
            my ($magnet_link) = ($args->{'Magnet'} =~ m[^magnet:\?(.+)$]);
            if (!$magnet_link) {
                carp q[Net::BitTorrent::Torrent->new({ }) was handed a ]
                    . q[malformed Magnet link according to BEP 009. (URL not ]
                    . q[in the form of magnet:?...)];
                return;
            }
            my $_xt = _url_param($magnet_link, 'xt');
            if (!$_xt) {
                carp q[Net::BitTorrent::Torrent->new({ }) was handed a ]
                    . q[malformed Magnet link according to BEP 009. (URL does ]
                    . q[not contain parsable parameters)];
                return;
            }
            my @_xt = split m[:], $_xt;
            if (($_xt[0] ne 'urn') || ($_xt[1] ne 'btih')) {
                carp q[Net::BitTorrent::Torrent->new({ }) was handed a ]
                    . q[malformed Magnet link according to BEP 009.];
                return;
            }

            #warn pp \@_xt;
            my $_infohash;
            if (length($_xt[2]) == 32) {   # Stupid, 32bit encoded infohash...
                $infohash{refaddr $self} = unpack 'H40',
                    _decode_base32($_xt[2]);
            }
            elsif (length($_xt[2]) == 40) { # Clean, smart, 40bit infohash! :D
                $infohash{refaddr $self} = $_xt[2];
            }
            else {
                carp 'Fail';
                return;
            }
        }
        elsif ($args->{q[Path]}) {
            if (!-f $args->{q[Path]}) {
                carp
                    sprintf(
                       q[Net::BitTorrent::Torrent->new({ }) cannot find '%s'],
                       $args->{q[Path]});
                return;
            }
            $args->{q[Path]} = rel2abs($args->{q[Path]});
            my ($TORRENT_FH, $TORRENT_RAW);
            if (not sysopen($TORRENT_FH, $args->{q[Path]}, O_RDONLY)) {
                carp
                    sprintf(
                    q[Net::BitTorrent::Torrent->new({ }) could not open '%s': %s],
                    $args->{q[Path]}, $!);
                return;
            }
            flock($TORRENT_FH, LOCK_SH);
            if (sysread($TORRENT_FH, $TORRENT_RAW, -s $args->{q[Path]})
                != -s $args->{q[Path]})
            {   carp sprintf(
                    q[Net::BitTorrent::Torrent->new({ }) could not read all %d bytes of '%s' (Read %d instead)],
                    -s $args->{q[Path]},
                    $args->{q[Path]}, length($TORRENT_RAW)
                );
                return;
            }
            flock($TORRENT_FH, LOCK_UN);
            my $raw_data = bdecode($TORRENT_RAW);
            close($TORRENT_FH);
            undef $TORRENT_FH;
            undef $TORRENT_RAW;
            if (!$raw_data) {
                carp q[Malformed .torrent];
                return;
            }
            $metadata{refaddr $self} = $raw_data->{'info'};
            $path{refaddr $self}     = $args->{q[Path]};
            $self->_handle_metadata;    # TODO: || return; # with bad data msg
            foreach my $_tier (
                       $raw_data->{q[announce-list]}
                       ? @{$raw_data->{q[announce-list]}}
                       : $raw_data->{q[announce]} ? [$raw_data->{q[announce]}]
                       : ()
                )
            {   push(@{$trackers{refaddr $self}},
                     Net::BitTorrent::Torrent::Tracker->new(
                                            {Torrent => $self, URLs => $_tier}
                     )
                );
            }
            if (   ($args->{q[Client]})
                && (blessed $args->{q[Client]})
                && ($args->{q[Client]}->isa(q[Net::BitTorrent])))
            {   foreach my $_node ($raw_data->{q[nodes]}
                                   ? @{$raw_data->{q[nodes]}}
                                   : ()
                    )
                {   $args->{q[Client]}->_dht->add_node(
                                    {ip => $_node->[0], port => $_node->[1]});
                }
            }
        }
        else {
            carp
                q[Net::BitTorrent::Torrent->new({ }) requires a 'Path' or 'Magnet' link];
            return;
        }

        # We either have a Path or Magnet link by this time
        if (($args->{q[Client]})
            && (   (!blessed $args->{q[Client]})
                || (!$args->{q[Client]}->isa(q[Net::BitTorrent])))
            )
        {   carp q[Net::BitTorrent::Torrent->new({ }) requires a ]
                . q[blessed Net::BitTorrent object in the 'Client' parameter];
            return;
        }
        if (    $args->{q[BlockLength]}
            and $args->{q[BlockLength]} !~ m[^\d+$])
        {   carp q[Net::BitTorrent::Torrent->new({ }) requires an ]
                . q[integer 'BlockLength' parameter];
            delete $args->{q[BlockLength]};
        }

        #
        $_working_pieces{refaddr $self} = {};
        $_block_length{refaddr $self} = (defined $args->{q[BlockLength]}
                                         ? $args->{q[BlockLength]}
                                         : (2**14)
        );
        $downloaded{refaddr $self} = 0;
        $uploaded{refaddr $self}   = 0;
        $_nodes{refaddr $self}     = q[];
        ${$status{refaddr $self}} |= LOADED;
        ${$error{refaddr $self}} = undef;

        # Resume system v2
        my $_start = 1;
        $resume_path{refaddr $self} = undef;
        if ($args->{q[Resume]}) {
            $resume_path{refaddr $self} = $args->{q[Resume]};
            my $_resume_data;
            if (-f $args->{q[Resume]}) {
                open(my ($_RD), q[<], $resume_path{refaddr $self});
                sysread($_RD, $_resume_data, -s $_RD);
                close $_RD;
            }
            if ($_resume_data) {
                $_start       = 0;
                $_resume_data = bdecode($_resume_data);

                # Resume system
                if (   $_resume_data->{q[.format]}
                    && $_resume_data->{q[.format]} eq
                    q[Net::BitTorrent resume]
                    && $_resume_data->{q[.version]}
                    && $_resume_data->{q[.version]} <= 2    # apiver
                    )
                {   $_nodes{refaddr $self}
                        = $_resume_data->{q[peers]}
                        ? $_resume_data->{q[peers]}
                        : q[];
                    my $_okay = 1;
                    for my $_index (0 .. $#{$files{refaddr $self}}) {
                        if ((!-f $files{refaddr $self}->[$_index]->path
                             && $_resume_data->{q[files]}[$_index]{q[mtime]}
                            )
                            || ((stat($files{refaddr $self}->[$_index]->path))
                                [9]
                                || 0 != $_resume_data->{q[files]}[$_index]
                                {q[mtime]})
                            )
                        {   ${$status{refaddr $self}} |= START_AFTER_CHECK;
                            $_okay = 0;
                        }
                        $files{refaddr $self}->[$_index]->set_priority(
                             $_resume_data->{q[files]}[$_index]{q[priority]});
                    }
                    if (!$_okay) {
                        $self->_set_error(
                                       q[Bad resume data. Please hashcheck.]);
                    }
                    else {
                        ${$bitfield{refaddr $self}}
                            = $_resume_data->{q[bitfield]};

                        # Accept resume data is the same as hashchecking
                        my $start_after_check
                            = ${$status{refaddr $self}} & START_AFTER_CHECK;
                        ${$status{refaddr $self}} ^= START_AFTER_CHECK
                            if ${$status{refaddr $self}} & START_AFTER_CHECK;
                        ${$status{refaddr $self}} ^= CHECKED
                            if !(${$status{refaddr $self}} & CHECKED);
                        if ($start_after_check) { $_start = 1; }

                        # Reload Blocks
                        for my $_piece (@{$_resume_data->{q[working]}}) {
                            $_working_pieces{refaddr $self}
                                {$_piece->{q[Index]}} = {
                                Index            => $_piece->{q[Index]},
                                Priority         => $_piece->{q[Priority]},
                                Blocks_Requested => [
                                     map { {} } 1 .. $_piece->{q[Block_Count]}
                                ],
                                Blocks_Received => [
                                    map {
                                        vec($_piece->{q[Blocks_Received]},
                                            $_, 1)
                                        } 1 .. $_piece->{q[Block_Count]}
                                ],
                                Block_Length => $_piece->{q[Block_Length]},
                                Block_Length_Last =>
                                    $_piece->{q[Block_Length_Last]},
                                Block_Count => $_piece->{q[Block_Count]},
                                Length      => $_piece->{q[Length]},
                                Endgame     => $_piece->{q[Endgame]},
                                Slow  => 1,     # $_piece->{q[Slow]},
                                mtime => time
                                };
                        }
                    }
                }
            }
        }

        # Threads stuff
        weaken($REGISTRY{refaddr $self} = $self);
        if ($threads::shared::threads_shared) {
            threads::shared::share($bitfield{refaddr $self});
            threads::shared::share($status{refaddr $self});
            threads::shared::share($error{refaddr $self});
        }
        $$self = $infohash{refaddr $self};
        if ($args->{q[Client]}) {
            $self->queue($args->{q[Client]});
            $_client{refaddr $self}->_schedule(
                                     {Time   => time + 25,
                                      Code   => sub { shift->_dht_announce },
                                      Object => $self
                                     }
            );
            $_client{refaddr $self}->_schedule(
                                       {Time   => time,
                                        Code   => sub { shift->_dht_scrape },
                                        Object => $self
                                       }
            );
        }
        $self->start if $_start && (${$status{refaddr $self}} & QUEUED);
        $self->_new_peer();    # XXX - temporary multi-thread vs schedule fix
        return $self;
    }

    # Accessors | Public
    sub infohash    { return $infohash{refaddr +shift}; }
    sub trackers    { return $trackers{refaddr +shift}; }
    sub bitfield    { return ${$bitfield{refaddr +shift}}; }
    sub path        { return $path{refaddr +shift}; }
    sub resume_path { return $resume_path{refaddr +shift}; }
    sub files       { return $files{refaddr +shift}; }
    sub size        { return $size{refaddr +shift}; }
    sub status      { return ${$status{refaddr +shift}}; }
    sub downloaded  { return $downloaded{refaddr +shift}; }
    sub uploaded    { return $uploaded{refaddr +shift}; }
    sub error       { return ${$error{refaddr +shift}}; }
    sub name        { return $metadata{refaddr +shift}{'name'}; }
    sub private     { return $metadata{refaddr +shift}{'private'} ? 1 : 0; }

    sub metadata {
        my ($self, $raw) = @_;
        return if !(${$status{refaddr $self}} & HAVE_METADATA);
        return $raw
            ? $metadata{refaddr $self}
            : bencode $metadata{refaddr $self};
    }

    sub _metadata_size {
        my ($self) = @_;
        return if (${$status{refaddr $self}} & HAVE_METADATA);
        return $_metadata_size{refaddr $self};
    }

    sub _set_metadata_size {
        my ($self, $size) = @_;
        return if (${$status{refaddr $self}} & HAVE_METADATA);
        return $_metadata_size{refaddr $self} = $size;
    }

    sub _set_metadata_piece {
        my ($self, $index, $data) = @_;
        return if (${$status{refaddr $self}} & HAVE_METADATA);
        $_metadata_requests{refaddr $self}->[$index] = $data;
        if (!scalar $self->_metadata_unrequested_blocks) {

            # $metadata{refaddr $self}
            # TODO: hashcheck, load data, delete cache
            my $_all_data = join '', @{$_metadata_requests{refaddr $self}};
            delete $_metadata_requests{refaddr $self};    # clear cache
            $metadata{refaddr $self} = bdecode $_all_data;
            if ($infohash{refaddr $self} ne sha1_hex $_all_data) {

                # Data went bad somewhere along the way...
                return;
            }
            elsif (!$self->_handle_metadata()) {

                # TODO: error msg
                return;
            }

            #die $self->as_string(1);
        }
        return 1;
    }

    sub _metadata_register_request {
        my ($self, $index, $peer) = @_;
        return if (${$status{refaddr $self}} & HAVE_METADATA);
        return if $_metadata_requests{refaddr $self}->[$index];
        $_metadata_requests{refaddr $self}->[$index] = $peer;
        return weaken $_metadata_requests{refaddr $self}->[$index];
    }

    sub _metadata_unrequested_blocks {
        my ($self) = @_;
        return if (${$status{refaddr $self}} & HAVE_METADATA);
        return [0] if !$_metadata_size{refaddr $self};    # Could this happen?
        my @all_blocks
            = 0 ..
            int(  ($_metadata_size{refaddr $self} / (1024 * 16))
                + (($_metadata_size{refaddr $self} % (1024 * 16)) ? 0 : -1));
        return
            map { $_metadata_requests{refaddr $self}->[$_] ? () : $_ }
            @all_blocks;
    }

    sub is_complete {
        my ($self) = @_;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & HAVE_METADATA);
        return unpack(q[b*], $self->_wanted) !~ m[1] ? 1 : 0;
    }

    sub piece_count {    # XXX - cache?
        my ($self) = @_;
        return if !(${$status{refaddr $self}} & HAVE_METADATA);
        return
            int(
             length(unpack(q[H*], $metadata{refaddr $self}{q[pieces]})) / 40);
    }

    sub peers {
        my ($self) = @_;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        my $_connections = $_client{refaddr $self}->_connections;
        return map {
            (    ($_->{q[Object]}->isa(q[Net::BitTorrent::Peer]))
             and ($_->{q[Object]}->torrent)
             and ($_->{q[Object]}->torrent eq $self))
                ? $_->{q[Object]}
                : ()
        } values %$_connections;
    }

    # Mutators | Private
    sub _add_node {
        my ($self, $node) = @_;
        return $_nodes{refaddr $self} .= compact($node);
    }

    sub _set_bitfield {
        my ($self, $new_value) = @_;
        return if (${$status{refaddr $self}} & CHECKING);
        return if length ${$bitfield{refaddr $self}} != length $new_value;

        # XXX - make sure bitfield conforms to what we expect it to be
        return ${$bitfield{refaddr $self}} = $new_value;
    }

    sub _set_status {
        my ($self, $new_value) = @_;
        return if (${$status{refaddr $self}} & CHECKING);

        # XXX - make sure status conforms to what we expect it to be
        return ${$status{refaddr $self}} = $new_value;
    }

    sub _set_error {
        my ($self, $msg) = @_;
        ${$error{refaddr $self}} = $msg;
        $self->stop() if ${$status{refaddr $self}} & STARTED;
        ${$status{refaddr $self}} |= ERROR;
        return 1;
    }

    sub _set_block_length {
        my ($self, $value) = @_;
        return if $value !~ m[^\d+$];
        return $_block_length{refaddr $self} = $value;
    }

    # Accessors | Private
    sub _client         { return $_client{refaddr +shift}; }
    sub _block_length   { return $_block_length{refaddr +shift} }
    sub _nodes          { return $_nodes{refaddr +shift}; }
    sub _working_pieces { return $_working_pieces{refaddr +shift}; }
    sub _basedir        { return $_basedir{refaddr +shift}; }

    sub _wanted {
        my ($self) = @_;
        return if !(${$status{refaddr $self}} & HAVE_METADATA);
        my $wanted = q[0] x $self->piece_count;
        my $p_size = $metadata{refaddr $self}{q[piece length]};
        my $offset = 0;
        for my $file (@{$files{refaddr $self}}) {
            my $start = ($offset / $p_size);
            my $end   = (($offset + $file->size) / $p_size);
            if ($file->priority ? 1 : 0) {
                substr($wanted, $start,
                       ($end - $start + 1),
                       (($file->priority ? 1 : 0) x ($end - $start + 1)));
            }
            $offset += $file->size;
        }
        return (
             pack(q[b*], $wanted)
                 | ${$bitfield{refaddr $self}} ^ ${$bitfield{refaddr $self}});
    }

    sub _weights {
        my ($self) = @_;
        my %_weights;
        my $p_size = $metadata{refaddr $self}{q[piece length]};
        my $offset = 0;
        for my $file (@{$files{refaddr $self}}) {
            my $priority = $file->priority;
            my $start    = ($offset / $p_size);
            my $end      = (($offset + $file->size) / $p_size);
            $offset += $file->size;
            next if !$priority;
            grep {
                $_weights{$_} = $priority
                    if !vec(${$bitfield{refaddr $self}}, $_, 1)
            } $start .. $end;
        }
        return %_weights;
    }

    # Methods | Public
    sub hashcheck {
        my ($self) = @_;
        return if (${$status{refaddr $self}} & PAUSED);
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & HAVE_METADATA);
        ${$bitfield{refaddr $self}}    # empty it first
            = pack(q[b*], qq[\0] x $self->piece_count);
        my $start_after_check = ${$status{refaddr $self}} & START_AFTER_CHECK;
        ${$status{refaddr $self}} |= CHECKING
            if !${$status{refaddr $self}} & CHECKING;

        for my $index (0 .. ($self->piece_count - 1)) {
            $self->_check_piece_by_index($index);
        }
        (${$status{refaddr $self}} ^= START_AFTER_CHECK)
            if ${$status{refaddr $self}} & START_AFTER_CHECK;
        ${$status{refaddr $self}} ^= CHECKED
            if !(${$status{refaddr $self}} & CHECKED);
        ${$status{refaddr $self}} ^= CHECKING
            if ${$status{refaddr $self}} & CHECKING;
        if ($start_after_check) { $self->start(); }
        return 1;
    }

    sub pause {
        my ($self) = @_;
        if (!${$status{refaddr $self}} & QUEUED) {
            carp q[Cannot pause an orphan torrent];
            return;
        }
        if (!${$status{refaddr $self}} & STARTED) {
            carp q[Cannot pause a stopped torrent];
            return;
        }
        return ${$status{refaddr $self}} |= PAUSED;
    }

    sub start {
        my ($self) = @_;
        return if !(${$status{refaddr $self}} & QUEUED);
        ${$status{refaddr $self}} ^= ERROR
            if ${$status{refaddr $self}} & ERROR;
        ${$status{refaddr $self}} ^= PAUSED
            if ${$status{refaddr $self}} & PAUSED;
        if (!(${$status{refaddr $self}} & STARTED)) {
            ${$status{refaddr $self}} |= STARTED;
            for my $tracker (@{$trackers{refaddr $self}}) {
                $tracker->_announce(q[started]);
            }
        }
        return ${$status{refaddr $self}};
    }

    sub stop {
        my ($self) = @_;
        return if !(${$status{refaddr $self}} & QUEUED);
        for my $_peer ($self->peers) {
            $_peer->_disconnect(q[Torrent has been stopped]);
        }
        for my $_file (@{$files{refaddr $self}}) { $_file->_close(); }
        if (${$status{refaddr $self}} & STARTED) {
            ${$status{refaddr $self}} ^= STARTED;
            for my $tracker (@{$trackers{refaddr $self}}) {
                $tracker->_announce(q[stopped]);
            }
        }
        return !!${$status{refaddr $self}} & STARTED;
    }

    sub queue {
        my ($self, $client) = @_;
        if (   (!$client)
            || (!blessed $client)
            || (!$client->isa(q[Net::BitTorrent])))
        {   carp q[Net::BitTorrent::Torrent->queue() requires a ]
                . q[blessed Net::BitTorrent object];
            return;
        }
        if ($_client{refaddr $self} or ${$status{refaddr $self}} & QUEUED) {
            carp q[Cannot serve the same .torrent more than once];
            return;
        }
        $_client{refaddr $self} = $client;
        weaken $_client{refaddr $self};
        ${$status{refaddr $self}} ^= QUEUED;

        #$self->_new_peer();
        return $_client{refaddr $self};
    }

    # Methods | Private
    sub _add_uploaded {
        my ($self, $amount) = @_;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        return if not $amount;
        $uploaded{refaddr $self} += (($amount =~ m[^\d+$]) ? $amount : 0);
    }

    sub _add_downloaded {
        my ($self, $amount) = @_;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        $downloaded{refaddr $self} += (($amount =~ m[^\d+$]) ? $amount : 0);
    }

    sub _new_peer {
        my ($self) = @_;
        return if not defined $_client{refaddr $self};
        $_client{refaddr $self}->_schedule(
                             {Time => time + ($self->is_complete ? 60 : 5),
                              Code => sub { shift->_new_peer if @_; },
                              Object => $self
                             }
        );
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & STARTED);
        return if !(${$status{refaddr $self}} & QUEUED);

        # Don't bother if we're at the hard limit
        return
            if scalar $self->peers
                >= $_client{refaddr $self}->_peers_per_torrent;

        #
        my $half_open = scalar(
            grep {
                $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                    and not defined $_->{q[Object]}->peerid
                } values %{$_client{refaddr $self}->_connections}
        );

        #warn sprintf q[%d half open peers], $half_open;
        # List of peers to make sure we're not already connected to this peer
        my @peers = $self->peers;

        # If we haven't any nodes in cache, gather them from various sources
        if (!$_nodes{refaddr $self}) {
            $_nodes{refaddr $self}
                = $_client{refaddr $self}->_dht->_peers($self->infohash)
                if !$self->private;
            for my $tier (@{$trackers{refaddr $self}}) {
                for my $url (@{$tier->urls}) {
                    $_nodes{refaddr $self} .= $url->_peers;
                }
            }
        }

        # Don't bother if we haven't any nodes to try
        return if !$_nodes{refaddr $self};

        # Inflate the list and try them one-by-one
        my @nodes = uncompact($_nodes{refaddr $self});
        for ($half_open .. $_client{refaddr $self}->_half_open - 1) {
            last if !@nodes;
            my $node = shift @nodes;
            next
                if scalar grep {
                sprintf(q[%s:%d], ($_->host || q[]), ($_->port || 0)) eq
                    $node    # already connected to this peer
                } @peers;
            my $ok = $_client{refaddr $self}
                ->_event(q[ip_filter], {Address => $node});
            if (defined $ok and $ok == 0) { next; }
            my $peer =
                Net::BitTorrent::Peer->new({Address => $node,
                                            Torrent => $self,
                                            Source  => q[TODO]
                                           }
                );
        }

        # Store only nodes we haven't tried yet
        $_nodes{refaddr $self} = compact(@nodes);

        # Return
        return 1;
    }

    sub _add_tracker {
        my ($self, $tier) = @_;
        carp q[Please, pass new tier in an array ref...]
            unless ref $tier eq q[ARRAY];
        my $tracker = Net::BitTorrent::Torrent::Tracker->new(
                                           {Torrent => $self, URLs => $tier});
        $tracker->_announce(q[started]);
        return push(@{$trackers{refaddr $self}}, $tracker);
    }

    sub _piece_by_index {
        my ($self, $index) = @_;
        return if !${$status{refaddr $self}} & STARTED;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        if ((!defined $index) || ($index !~ m[^\d+$])) {
            carp
                q[Net::BitTorrent::Torrent->_piece_by_index() requires an index];
            return;
        }
        return $_working_pieces{refaddr $self}{$index}
            ? $_working_pieces{refaddr $self}{$index}
            : ();
    }

    sub _pick_piece {
        my ($self, $peer) = @_;
        return if $self->is_complete;
        return if !${$status{refaddr $self}} & STARTED;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        if (!$_client{refaddr $self}) {
            carp
                q[Net::BitTorrent::Torrent->_pick_piece(PEER) will not on an orphan torrent];
            return;
        }
        if (   (!${$status{refaddr $self}} & STARTED)
            || (${$status{refaddr $self}} & CHECKING))
        {   carp
                q[Net::BitTorrent::Torrent->_pick_piece(PEER) will not work while hashchecking];
            return;
        }
        if (   (!$peer)
            || (!blessed $peer)
            || (!$peer->isa(q[Net::BitTorrent::Peer])))
        {   carp
                q[Net::BitTorrent::Torrent->_pick_piece(PEER) requires a peer];
            return;
        }
        my $piece;

        # Pseudo
        #   if ( num_working_pieces < max_working_pieces )
        #       index = callback
        #       if not defined index
        #           index = rand(indexes_we_do_not_have)
        #   return if not defined index
        #   piece = working_piece{index} || new piece(index)
        #   return defined piece ? piece : undef
        # my $index = $self->_event(q[piece_selection]);
        my $_wanted   = $self->_wanted;
        my $x         = -1;
        my @relevence = map { $x++; $_ ? $x : () }
            split('', unpack 'b*', ($peer->bitfield & $_wanted));
        return if !scalar @relevence;
        my $endgame = (    # XXX - static ratio
            (sum(split(q[], unpack(q[b*], $_wanted)))
                 <= (length(unpack(q[b*], $_wanted)) * .1)
            ) ? 1 : 0
        );

        #warn sprintf q[Endgame | %d <= %d (%d) ? %d],
        #    sum(split(q[], unpack(q[b*], $_wanted))),
        #    (length(unpack(q[b*], $_wanted)) * .1),
        #    length(unpack(q[b*], $_wanted)),
        #    $endgame;
        my $unrequested_blocks = 0;
        for my $index (keys %{$_working_pieces{refaddr $self}}) {
            $unrequested_blocks += scalar grep {
                !keys %{$_working_pieces{refaddr $self}{$index}
                        {q[Blocks_Requested]}[$_]}
                } 0 .. $_working_pieces{refaddr $self}{$index}{q[Block_Count]}
                - 1;
        }
        if (scalar(grep { $_->{q[Slow]} == 1 }
                       values %{$_working_pieces{refaddr $self}}
            ) >= 3
            )
        {   my @indexes
                = grep { $_working_pieces{refaddr $self}{$_}{q[Slow]} == 1 }
                keys %{$_working_pieces{refaddr $self}};
            for my $index (@indexes) {
                if (grep { $_ == $index } @relevence) {
                    if (($endgame
                         ? index($_working_pieces{refaddr $self}{$index}
                                     {q[Blocks_Received]},
                                 0,
                                 0
                         )
                         : scalar grep { scalar keys %$_ }
                         @{  $_working_pieces{refaddr $self}{$index}
                                 {q[Blocks_Requested]}
                         }
                        ) != -1
                        )
                    {   $piece = $_working_pieces{refaddr $self}{$index};
                        last;
                    }
                }
            }
        }
        elsif (scalar(values %{$_working_pieces{refaddr $self}}) >= (
                         ($unrequested_blocks > (
                                 int($metadata{refaddr $self}{q[piece length]}
                                         / $_block_length{refaddr $self}
                                     ) / 4
                              ) ? 0 : 1
                         ) + scalar keys %{$_working_pieces{refaddr $self}}
               )
            )
        {   my @indexes = sort {
                (scalar grep { scalar keys %$_ }
                     @{
                     $_working_pieces{refaddr $self}{$a}{q[Blocks_Requested]}
                     }
                    ) <=> (scalar grep { scalar keys %$_ }
                               @{
                               $_working_pieces{refaddr $self}{$b}
                                   {q[Blocks_Requested]}
                               }
                    )
            } keys %{$_working_pieces{refaddr $self}};
            for my $index (@indexes) {
                if (grep { $_ == $index } @relevence) {
                    if (($endgame
                         ? index($_working_pieces{refaddr $self}{$index}
                                     {q[Blocks_Received]},
                                 0,
                                 0
                         )
                         : scalar grep { scalar keys %$_ }
                         @{  $_working_pieces{refaddr $self}{$index}
                                 {q[Blocks_Requested]}
                         }
                        ) != -1
                        )
                    {   $piece = $_working_pieces{refaddr $self}{$index};
                        last;
                    }
                }
            }
        }
        else {
            my %weights = $self->_weights;
            return if not keys %weights;
            my $total    = sum values %weights;    # [id://230661]
            my $rand_val = $total * rand;
            my $index;
            for my $i (reverse sort keys %weights) {
                $rand_val -= $weights{$i};
                if ($rand_val <= 0
                    && (grep { $_ == $i } @relevence))
                {   $index = $i;
                    last;
                }
            }
            return if not defined $index;
            my $_piece_length = (    # XXX - save some time and cache this?
                ($index == int(      $size{refaddr $self}
                                   / $metadata{refaddr $self}{q[piece length]}
                 )
                )
                ? ($size{refaddr $self} % $metadata{refaddr $self}
                   {q[piece length]})
                : ($metadata{refaddr $self}{q[piece length]})
            );
            my $block_length = (($metadata{refaddr $self}{q[piece length]}
                                     < $_block_length{refaddr $self}
                                )
                                ? ($metadata{refaddr $self}{q[piece length]})
                                : $_block_length{refaddr $self}
            );
            my $block_length_last = (
                  $metadata{refaddr $self}{q[piece length]} % $_piece_length);
            my $block_count
                = (int($_piece_length / $block_length)
                       + ($block_length_last ? 1 : 0));
            $piece = {Index             => $index,
                      Priority          => $weights{$index},
                      Blocks_Requested  => [map { {} } 1 .. $block_count],
                      Blocks_Received   => [map {0} 1 .. $block_count],
                      Block_Length      => $block_length,
                      Block_Length_Last => $block_length_last,
                      Block_Count       => $block_count,
                      Length            => $_piece_length,
                      Endgame           => $endgame,
                      Slow              => 1,
                      mtime             => 0
            };
        }
        if ($piece) {
            if (not
                defined $_working_pieces{refaddr $self}{$piece->{q[Index]}})
            {   $_working_pieces{refaddr $self}{$piece->{q[Index]}} = $piece;
                $_working_pieces{refaddr $self}{$piece->{q[Index]}}
                    {q[Endgame]} = $endgame;
            }
        }
        return $piece
            ? $_working_pieces{refaddr $self}{$piece->{q[Index]}}
            : ();
    }

    sub _write_data {
        my ($self, $index, $offset, $data) = @_;
        return if !${$status{refaddr $self}} & STARTED;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        if ((length($$data)
             + (($metadata{refaddr $self}{q[piece length]} * $index)
                 + $offset)
            ) > $size{refaddr $self}
            )
        {   carp q[Too much data or bad offset data for this torrent];
            return;
        }
        my $file_index = 0;
        my $total_offset
            = int(  (($index * $metadata{refaddr $self}{q[piece length]}))
                  + ($offset || 0));
    SEARCH:
        while ($total_offset > $files{refaddr $self}->[$file_index]->size) {
            $total_offset -= $files{refaddr $self}->[$file_index]->size;
            $file_index++;
            last SEARCH    # XXX - return?
                if not defined $files{refaddr $self}->[$file_index]->size;
        }
    WRITE: while (length $$data > 0) {
            my $this_write
                = ($total_offset + length $$data
                   > $files{refaddr $self}->[$file_index]->size)
                ? $files{refaddr $self}->[$file_index]->size - $total_offset
                : length $$data;
            $files{refaddr $self}->[$file_index]->_open(q[w]) or return;
            $files{refaddr $self}->[$file_index]->_sysseek($total_offset);
            $files{refaddr $self}->[$file_index]
                ->_write(substr($$data, 0, $this_write, q[]))
                or return;
            $file_index++;
            last WRITE
                if not defined $files{refaddr $self}->[$file_index];
            $total_offset = 0;
        }
        return 1;
    }

    sub _read_data {
        my ($self, $index, $offset, $length) = @_;
        return if !defined $index  || $index !~ m[^\d+$];
        return if !defined $offset || $offset !~ m[^\d+$];
        return if !defined $length || $length !~ m[^\d+$];
        my $data = q[];
        if (($length
             + (($metadata{refaddr $self}{q[piece length]} * $index)
                 + $offset)
            ) > $size{refaddr $self}
            )
        {   carp q[Too much or bad offset data for this torrent];
            return;
        }
        my $file_index = 0;
        my $total_offset
            = int(  (($index * $metadata{refaddr $self}{q[piece length]}))
                  + ($offset || 0));
    SEARCH:
        while ($total_offset > $files{refaddr $self}->[$file_index]->size) {
            $total_offset -= $files{refaddr $self}->[$file_index]->size;
            $file_index++;
            last SEARCH    # XXX - return?
                if not defined $files{refaddr $self}->[$file_index]->size;
        }
    READ: while ((defined $length) && ($length > 0)) {
            my $this_read
                = (($total_offset + $length)
                   >= $files{refaddr $self}->[$file_index]->size)
                ? ($files{refaddr $self}->[$file_index]->size - $total_offset)
                : $length;
            $files{refaddr $self}->[$file_index]->_open(q[r]) or return;
            $files{refaddr $self}->[$file_index]->_sysseek($total_offset);
            my $_data
                = $files{refaddr $self}->[$file_index]->_read($this_read);
            $data .= $_data if $_data;
            $file_index++;
            $length -= $this_read;
            last READ if not defined $files{refaddr $self}->[$file_index];
            $total_offset = 0;
        }
        return \$data;
    }

    sub _check_piece_by_index {
        my ($self, $index) = @_;
        if ((!defined $index) || ($index !~ m[^\d+$])) {
            carp q[Net::BitTorrent::Torrent->_check_piece_by_index( INDEX ) ]
                . q[requires an index.];
            return;
        }
        delete $_working_pieces{refaddr $self}{$index};
        my $data =
            $self->_read_data(
                           $index, 0,
                           ($index == ($self->piece_count - 1)
                            ? ($size{refaddr $self} % $metadata{refaddr $self}
                               {q[piece length]})
                            : $metadata{refaddr $self}{q[piece length]}
                           )
            );
        if ((!$data)
            or (sha1_hex($$data) ne substr(
                                       unpack(
                                           q[H*],
                                           $metadata{refaddr $self}{q[pieces]}
                                       ),
                                       $index * 40,
                                       40
                )
            )
            )
        {   vec(${$bitfield{refaddr $self}}, $index, 1) = 0;
            $self->_event(q[piece_hash_fail],
                          {Torrent => $self, Index => $index});
            return 0;
        }
        if (vec(${$bitfield{refaddr $self}}, $index, 1) == 0) {
            vec(${$bitfield{refaddr $self}}, $index, 1) = 1;
            $self->_event(q[piece_hash_pass],
                          {Torrent => $self, Index => $index});
        }
        return 1;
    }

    # Methods | Private | DHT
    sub _dht_announce {
        my ($self) = @_;
        $_client{refaddr $self}->_schedule(
                                     {Time   => time + 120,
                                      Code   => sub { shift->_dht_announce },
                                      Object => $self
                                     }
        );
        return if !${$status{refaddr $self}} & STARTED;
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        return if $self->private;
        return if !$_client{refaddr $self}->_use_dht;
        $_client{refaddr $self}->_dht->_announce($self);
        $_client{refaddr $self}->_schedule(
            {   Time => time + 15,
                Code => sub {
                    my ($s) = @_;
                    $_client{refaddr $s}->_dht->_scrape($s)
                        if $_client{refaddr $s}->_use_dht;
                },
                Object => $self
            }
        );
    }

    sub _dht_scrape {
        my ($self) = @_;
        $_client{refaddr $self}->_schedule(
                                       {Time   => time + 60,
                                        Code   => sub { shift->_dht_scrape },
                                        Object => $self
                                       }
        );
        return if !(${$status{refaddr $self}} & STARTED);
        return if (${$status{refaddr $self}} & CHECKING);
        return if !(${$status{refaddr $self}} & QUEUED);
        return if $self->private;
        $_client{refaddr $self}->_dht->_scrape($self)
            if $_client{refaddr $self}->_use_dht;
    }

    # Methods | Public | Callback system
    sub on_event {
        my ($self, $type, $method) = @_;
        carp sprintf q[Unknown callback: %s], $type
            unless ___check_event($type);
        $_event{refaddr $self}{$type} = $method;
    }

    # Methods | Private | Callback system
    sub _event {
        my ($self, $type, $args) = @_;
        carp sprintf
            q[Unknown event: %s. This is a bug in Net::BitTorrent::Torrent; Report it.],
            $type
            unless ___check_event($type);
        $_client{refaddr $self}->_event($type, $args)
            if ${$status{refaddr $self}} & QUEUED;
        return $_event{refaddr $self}{$type}
            ? $_event{refaddr $self}{$type}($self, $args)
            : ();
    }

    # Functions | Private | Callback system
    sub ___check_event {
        my $type = shift;
        return scalar grep { $_ eq $type } qw[
            tracker_connect tracker_disconnect
            tracker_read    tracker_write
            tracker_success tracker_failure
            piece_hash_pass piece_hash_fail
            file_open       file_close
            file_read       file_write
            file_error
        ];
    }

    # Methods | Public | Alpha
    sub save_resume_data {
        my ($self, $file) = @_;
        $file ||= $resume_path{refaddr $self};
        return if !$file;    # Don't even bother without a file to write to

        # Make sure file handles are closed so we don't mess up 'mtime' times
        for my $_file (@{$files{refaddr $self}}) { $_file->_close }

        # Gather nodes from various sources
        #   Internal
        my $_nodes = $_nodes{refaddr $self};

        #   DHT
        $_nodes .= (((${$status{refaddr $self}} & QUEUED) && !$self->private)
                    ? $_client{refaddr $self}->_dht->_peers($self->infohash)
                    : q[]
        );

        #   Trackers
        for my $tier (@{$trackers{refaddr $self}}) {
            for my $url (@{$tier->urls}) { $_nodes .= $url->_peers; }
        }

        # The resume data proper
        my %resume_data = (
            q[.format]  => q[Net::BitTorrent resume],
            q[.t]       => time,
            q[.version] => 2,
            bitfield    => ${$bitfield{refaddr $self}},
            files       => [
                map {
                    {priority => $_->priority,
                     mtime    => (-f $_->path ? (stat($_->path))[9] : 0)
                    }
                    } @{$files{refaddr $self}}
            ],
            peers => ($_nodes ? $_nodes : q[]),
            working => [
                map {
                    {Block_Count => $_->{q[Block_Count]},
                     Endgame     => $_->{q[Endgame]},
                     Blocks_Received =>
                         pack(q[b*], join q[], @{$_->{q[Blocks_Received]}}),
                     Index             => $_->{q[Index]},
                     Slow              => $_->{q[Slow]},
                     Block_Length      => $_->{q[Block_Length]},
                     Block_Length_Last => $_->{q[Block_Length_Last]},
                     Length            => $_->{q[Length]},
                     Priority          => $_->{q[Priority]}
                    }
                    } values %{$_working_pieces{refaddr $self}}
            ]
        );

        # Write it to disk
        open(my ($_RD), q[>], $file) || return;
        syswrite($_RD, bencode(\%resume_data)) || return;
        return close $_RD;
    }

    # Methods | Public | Utility
    sub as_string {
        my ($self, $advanced) = @_;
        my $wanted = $self->_wanted;
        my $dump
            = !$advanced ? $self->infohash : sprintf <<'END',
Net::BitTorrent::Torrent
Path:            %s
Name:            %s
Infohash:        %s
Base Directory:  %s
Size:            %s
Status:          %d (%s.)
DHT Status:      %s
Progress:        %3.2f%% complete (%d bytes up / %d bytes down)
[%s]
----------
Pieces: %d x %d bytes
Working: %s
%s
----------
 ...has %d file%s:
  %s
----------
 ...has %d tracker tier%s:
  %s
----------
END
            (
            (!(${$status{refaddr $self}} & HAVE_METADATA))
            ? (q[N/A],    # path
               q[N/A],    # name | TODO: use Magnet link 'dn' if it exists
               $self->infohash(),
               $_basedir{refaddr $self},
               q[N/A],    # size
               ${$status{refaddr $self}}, $self->_status_as_string(),
               ($self->private ? q[Disabled [Private]] : q[Enabled.]),
               0, 0, 0, q[N/A],    # complete, up, down, chart
               0,      0,          # pieces
               q[N/A], q[],        #working
               0,      q[], q[N/A] # has %d file%s:
                )
            : ($self->path,
               (       $metadata{refaddr $self}{q[name.utf-8]}
                    || $metadata{refaddr $self}{q[name]}
               ),
               $self->infohash(),
               $_basedir{refaddr $self},
               $size{refaddr $self} . ' bytes',
               ${$status{refaddr $self}},
               $self->_status_as_string(),
               ($self->private ? q[Disabled [Private]] : q[Enabled.]),
               100 - (grep {$_} split //,
                      unpack(q[b*], $wanted) / $self->piece_count * 100
               ),
               $uploaded{refaddr $self},
               $downloaded{refaddr $self},
               (sprintf q[%s],
                join q[],
                map {
                    vec(${$bitfield{refaddr $self}}, $_, 1) ? q[|]    # have
                        : $_working_pieces{refaddr $self}{$_} ? q[*] # working
                        : vec($wanted, $_, 1) ? q[ ]                 # missing
                        : q[x]    # don't want
                    } 0 .. $self->piece_count - 1
               ),
               $self->piece_count(),
               $metadata{refaddr $self}{q[piece length]},
               (scalar keys %{$_working_pieces{refaddr $self}} || q[N/A]),
               (join qq[\n],
                map {
                    my $index = $_;
                    sprintf q[%4d [%s] % 3.2f%%], $index, join(
                        q[],
                        map {
                            $_working_pieces{refaddr $self}{$index}
                                {q[Blocks_Received]}[$_] ? q[|]
                                : scalar
                                keys %{$_working_pieces{refaddr $self}{$index}
                                    {q[Blocks_Requested]}[$_]} == 1 ? q[*]
                                : scalar
                                keys %{$_working_pieces{refaddr $self}{$index}
                                    {q[Blocks_Requested]}[$_]} ? q[!]
                                : q[ ]
                            } 0 .. $_working_pieces{refaddr $self}{$index}
                            {q[Block_Count]} - 1
                        ),
                        (scalar(grep {$_}
                                    @{
                                    $_working_pieces{refaddr $self}{$index}
                                        {q[Blocks_Received]}
                                    }
                             )
                             / $_working_pieces{refaddr $self}{$index}
                             {q[Block_Count]}
                        ) * 100;
                    } sort { $a <=> $b }
                    keys %{$_working_pieces{refaddr $self}}
               ),
               scalar @{$files{refaddr $self}},
               @{$files{refaddr $self}} != 1 ? q[s] : q[],
               join(qq[\n  ], map { $_->path } @{$files{refaddr $self}}),
            )
            ),
            scalar @{$trackers{refaddr $self}},
            @{$trackers{refaddr $self}} != 1 ? q[s] : q[],
            join(qq[\n  ],
                 map     { $_->url }
                     map { @{$_->urls} } @{$trackers{refaddr $self}}
            );
        return defined wantarray ? $dump : print STDERR qq[$dump\n];
    }

    sub _status_as_string {    # TODO - HAVE_METADATA
        my ($self) = @_;
        return ucfirst join q[, ],
            grep {$_}
            (${$status{refaddr $self}} & LOADED) ? q[was loaded okay]
            : q[], (${$status{refaddr $self}} & STARTED) ? q[is started]
            : q[is stopped],
            (${$status{refaddr $self}} & CHECKING)
            ? q[is currently hashchecking]
            : q[],
            (${$status{refaddr $self}} & START_AFTER_CHECK)
            ? q[needs hashchecking]
            : q[], (${$status{refaddr $self}} & CHECKED) ? q[has been checked]
            : q[has not been checked],
            (${$status{refaddr $self}} & PAUSED) ? q[has been paused]
            : q[], (${$status{refaddr $self}} & QUEUED) ? q[is queued]
            : q[is good for informational use only],
            (${$status{refaddr $self}} & ERROR) ? q[but has an error]
            :                                     q[];
    }

    sub _handle_metadata {
        my ($self) = @_;
        return
            if length(unpack(q[H*], $metadata{refaddr $self}{q[pieces]}))
                < 40;
        return
            if length(unpack(q[H*], $metadata{refaddr $self}{q[pieces]}))
                % 40;
        $_metadata_size{refaddr $self}
            = length bencode($metadata{refaddr $self});
        $infohash{refaddr $self}
            = sha1_hex(bencode($metadata{refaddr $self}));
        my @_files;

        #warn pp $metadata{refaddr $self};
        if (defined $metadata{refaddr $self}{q[files]}) {
            for my $file (@{$metadata{refaddr $self}{q[files]}}) {
                push @_files,
                    [catfile($_basedir{refaddr $self},
                             (  $metadata{refaddr $self}{q[name.utf-8]}
                              ? $metadata{refaddr $self}{q[name.utf-8]}
                              : $metadata{refaddr $self}{q[name]}
                             ),
                             @{    $file->{q[path.utf-8]}
                                 ? $file->{q[path.utf-8]}
                                 : $file->{q[path]}
                                 }
                     ),
                     $file->{q[length]}
                    ];
            }
        }
        else {
            push @_files,
                [catfile($_basedir{refaddr $self},
                         (  $metadata{refaddr $self}{q[name.utf-8]}
                          ? $metadata{refaddr $self}{q[name.utf-8]}
                          : $metadata{refaddr $self}{q[name]}
                         )
                 ),
                 $metadata{refaddr $self}{q[length]}
                ];
        }
        $size{refaddr $self} = 0;
        for my $_file (@_files) {
            my ($path, $size) = @$_file;
            $path =~ s[\.\.][]g;
            $path =~ m[(.+)];
            $path = $1;
            utf8::upgrade($path) if $path =~ m[[[:^print:]]];    # ugh...
            push(@{$files{refaddr $self}},
                 Net::BitTorrent::Torrent::File->new(
                                 {Size    => $size,
                                  Path    => $path,
                                  Torrent => $self,
                                  Index   => scalar(@{$files{refaddr $self}})
                                 }
                 )
            );
            $size{refaddr $self} += $size;
        }

        # Everything's good.
        ${$status{refaddr $self}} |= HAVE_METADATA;
        ${$bitfield{refaddr $self}}
            = pack(q[b*], qq[\0] x $self->piece_count);
        return 1;
    }

    sub CLONE {
        for my $_oID (keys %REGISTRY) {
            my $_obj = $REGISTRY{$_oID};
            my $_nID = refaddr $_obj;
            for (@CONTENTS) {
                $_->{$_nID} = $_->{$_oID};
                delete $_->{$_oID};
            }
            weaken $_client{$_nID};
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }
    DESTROY {
        my ($self) = @_;
        for (@CONTENTS) { delete $_->{refaddr $self}; }
        return delete $REGISTRY{refaddr $self};
    }

    # utility functions
    sub _decode_base32 {
        %_char2bits = sub {
            my $_x = 0;
            return map { $_ => sprintf '%05b', $_x++ } ('a' .. 'z', 2 .. 7);
            }
            ->() if !%_char2bits;
        pack 'B*', join '', map { $_char2bits{$_} } split '', lc shift;
    }

    sub _url_param {    # 'Inspired' by CGI::url_param()
        my ($url, $name) = @_;
        $url || return;
        my %param;
        if ($url =~ m[=]) {
            for (split(m[[&;]], $url)) {
                my ($param, $value) = split('=', $_, 2);
                push(@{$param{$param}}, $value);
            }
        }
        return keys %param unless defined($name);
        return () unless $param{$name};
        return wantarray
            ? @{$param{$name}}
            : $param{$name}->[0];
    }
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Torrent - Class Representing a Single .torrent File

=head1 Synopsis

  use Net::BitTorrent::Torrent;

  my $torrent = Net::BitTorrent::Torrent->new({Path => q[a.legal.torrent]})
      or die q[Cannot load .torrent];

  $torrent->on_event(
      q[piece_hash_pass],
      sub {
          printf qq[%s is % 3.2f%% complete\r], $torrent->name,
              (scalar grep {$_} split q[], unpack q[b*], $torrent->bitfield)
              / $torrent->piece_count * 100;
      }
  );

  $torrent->hashcheck;    # Verify any existing data

=head1 Description

C<Net::BitTorrent::Torrent> objects are typically created by the
C<Net::BitTorrent> class.

Standalone C<Net::BitTorrent::Torrent> objects can be made for
informational use.  See L<new ( )|/"new ( { [ARGS] } )"> and
L<queue ( )|/"queue ( CLIENT )">.

=head1 Constructor

=over

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Torrent> object.  This constructor is
called by
L<Net::BitTorrent-E<gt>add_torrent( )|Net::BitTorrent/"add_torrent ( { ... } )">.

C<new( )> accepts arguments as a hash, using key-value pairs:

=over

=item C<BaseDir>

The root directory used to store the files related to this torrent.  This
directory is created if not preexisting.

This is an optional parameter.

Default: C<./> (Current working directory)

=item C<Client>

The L<Net::BitTorrent|Net::BitTorrent> object this torrent will
eventually be served from.

This is an optional parameter.

No default.  Without a defined parent client, his object is very limited
in capability.  Basic information and L<hash checking|/hashcheck> only.
Orphan objects are obviously not L<queued|/"status ( )"> automatically
and must be added to a client L<manually|/"queue ( CLIENT )">.

=item C<Path>

Filename of the .torrent file to load.

This is the only required parameter.

=item C<Resume>

The filename used to gather and store resume data.

This is an optional parameter.

No default.  Without a defined resume file, resume data will not be
written on calls to
L<save_resume_data ( )|/"save_resume_data ( [ PATH ] )"> without a
C<PATH> parameter.

=item C<Status>

Initial status of the torrent.  This parameter is ORed with the loaded
and queued (if applicable) values.

For example, you could set the torrent to automatically start after
L<hashcheck|/"hashcheck ( )"> with
C<{ [...] Status =E<gt> START_AFTER_CHECK, [...] }>.

To import all supported statuses into your namespace, use the
C<status> keyword.

This is an optional parameter.

Default: 1 (started)

See also: L<status ( )|/"status ( )">

Note: This is alpha code and may not work correctly.

=back

=back

=head1 Methods

=over

=item C<bitfield ( )>

Returns a bitfield representing the pieces that have been successfully
downloaded.

=item C<downloaded ( )>

Returns the total amount downloaded from remote peers since the client
started transferring data related to this .torrent.

See also: L<uploaded ( )|/"uploaded ( )">

=item C<error ( )>

Returns the most recent error that caused the software to set the
error L<status|/"status ( )">.  Torrents with active errors are
automatically stopped and must be L<started|/"start ( )">.

See also: L<status ( )|/"status ( )">, L<start ( )|/"start ( )">

=item C<files ( )>

Returns a list of
L<Net::BitTorrent::Torrent::File|Net::BitTorrent::Torrent::File> objects
representing all files contained in the related .torrent file.

=item C<hashcheck ( )>

Verifies the integrity of all L<files|Net::BitTorrent::Torrent::File>
associated with this torrent.

This is a blocking method; all processing will stop until this function
returns.

See also: L<bitfield ( )|/"bitfield ( )">, L<status ( )|/"status ( )">

=item C<infohash ( )>

Returns the 20 byte SHA1 hash used to identify this torrent internally,
with trackers, and with remote peers.

=item C<is_complete ( )>

Returns a bool value based on download progress.  Returns C<true> when we
have completed every L<file|Net::BitTorrent::Torrent::File> with a
priority above C<0>.  Otherwise, returns C<false>.

See also:
L<Net::BitTorrent::Torrent::File-E<gt>priority()|Net::BitTorrent::Torrent::File/"priority( )">

=item C<name ( )>

Returns the advisory name used when creating the related files on disk.

In a single file torrent, this is used as the filename by default.  In a
multiple file torrent, this is used as the containing directory for
related files.

=item C<on_event ( TYPE, CODEREF )>

Net::BitTorrent::Torrent provides per-torrent callbacks.  For example,
to catch all attempts to read from a file, use
C<$torrent-E<gt>on_event( 'file_read', \&on_read )>.  These per-
torrent callbacks are especially useful for standalone torrents.

See the L<Events|/Events> section for more.

=item C<path ( )>

Returns the L<filename|/"Path"> of the torrent this object represents.

=item C<pause ( )>

Pauses an active torrent without closing related sockets.

See also: L<status ( )|/"status ( )">, L<stop ( )|/"stop ( )">,
L<start ( )|/"start ( )">

=item C<peers ( )>

Returns a list of remote L<peers|Net::BitTorrent::Peer> related to this
torrent.

=item C<piece_count ( )>

The number of pieces this torrent's data is broken into.

=item C<private ( )>

Returns bool value dependent on whether the private flag is set in the
.torrent metadata.  Private torrents disallow information sharing via DHT
and PEX.

=item C<queue ( CLIENT )>

Adds a standalone (or orphan) torrent object to the particular
L<CLIENT|Net::BitTorrent> object's queue.

See also:
L<remove_torrent ( )|Net::BitTorrent/"remove_torrent ( TORRENT )">

=item C<metadata ( [ RAW ] )>

Returns the bencoded metadata found in the .torrent file. This method
returns the original metadata in either bencoded form or as a raw hash
(if you have other plans for the data) depending on the boolean value of
the optional C<RAW> parameter.

=item C<resume_path ( )>

Returns the default path used to
L<store resume data|/"save_resume_data ( [ PATH ] )">.  This value is set
in the C<Resume> parameter to L<new|/"new ( { [ARGS] } )">.

=item C<save_resume_data ( [ PATH ] )>

One end of Net::BitTorrent's resume system.  This method writes the
data to the file specified in the call to L<new( )|/"new ( { [ARGS] } )">
or (if defined) to the C<PATH> parameter.

See also:
L<Resume API|Net::BitTorrent::Notes/"Resume API">
and
L<How do I quick Resume a .torrent Session Between Client Sessions?|Net::BitTorrent::Notes/"Quick Resume a .torrent Session Between Client Sessions">
in L<Net::BitTorrent::Notes|Net::BitTorrent::Notes>

=item C<size ( )>

Returns the total size of all files listed in the .torrent file.

=item C<status ( )>

Returns the internal status of this C<Net::BitTorrent::Torrent> object.
States are bitwise C<AND> values of...

=begin html

 <table summary="List of possible states">
      <thead>
        <tr>
          <td>
            Value
          </td>
          <td>
            Type
          </td>
          <td>
            Notes
          </td>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>
            1
          </td>
          <td>
            STARTED
          </td>
          <td>
            Client is (making an attempt to be) active in the swarm
          </td>
        </tr>
        <tr>
          <td>
            2
          </td>
          <td>
            CHECKING
          </td>
          <td>
            Currently hashchecking (possibly in another thread)
          </td>
        </tr>
        <tr>
          <td>
            4
          </td>
          <td>
            START_AFTER_CHECK
          </td>
          <td>
            (Unused in this version)
          </td>
        </tr>
        <tr>
          <td>
            8
          </td>
          <td>
            CHECKED
          </td>
          <td>
            Files of this torrent have been checked
          </td>
        </tr>
        <tr>
          <td>
            16
          </td>
          <td>
            ERROR
          </td>
          <td>
            Activity is halted and may require user intervention
            (Unused in this version)
          </td>
        </tr>
        <tr>
          <td>
            32
          </td>
          <td>
            PAUSED
          </td>
          <td>
            Sockets are kept open but no piece data is sent or requested
          </td>
        </tr>
        <tr>
          <td>
            64
          </td>
          <td>
            LOADED
          </td>
          <td>
            Torrent has been parsed without error
          </td>
        </tr>
        <tr>
          <td>
            128
          </td>
          <td>
            QUEUED
          </td>
          <td>
            Has an associated Net::BitTorrent parent
          </td>
        </tr>
        <tr>
          <td>
            256
          </td>
          <td>
            HAVE_METADATA
          </td>
          <td>
            We have the metadata
          </td>
        </tr>
      </tbody>
    </table>

=end html

=begin :text,wiki

   1 = STARTED  (Client is (making an attempt to be) active in the swarm)
   2 = CHECKING (Currently hashchecking (possibly in another thread))
   4 = START_AFTER_CHECK*
   8 = CHECKED  (Files of this torrent have been checked)
  16 = ERROR    (Activity is halted and may require user intervention)
  32 = PAUSED   (Sockets are kept open but no piece data is sent or requested)
  64 = LOADED   (Torrent has been parsed without error)
 128 = QUEUED   (Has an associated Net::BitTorrent parent)
 256 = HAVE_METADATA (We have the metadata for this torrent)

 * Currently unused

=end :text,wiki

For example, a status of C<201> implies the torrent is
C<QUEUED | LOADED | CHECKED | STARTED>.

When torrents have the a status that indicates an error, they must be
L<restarted|/start ( )> (if possible).  The reason for the error I<may>
be returned by L<error ( )|/"error ( )">.

Import the C<:status> tag and you'll get the various status keywords in
your namespace.

=begin :podcoverage

=over

=item STARTED

=item CHECKING

=item START_AFTER_CHECK

=item CHECKED

=item ERROR

=item PAUSED

=item LOADED

=item QUEUED

=item HAVE_METADATA

=back

=end :podcoverage

Note: This is alpha and may not work as advertised.  Yet.

=item C<start ( )>

Starts a paused or stopped torrent.

See also: L<status ( )|/"status ( )">, L<stop ( )|/"stop ( )">,
L<pause ( )|/"pause ( )">

=item C<stop ( )>

Stops an active or paused torrent.  All related sockets (peers) are
disconnected and all files are closed.

See also: L<status ( )|/"status ( )">, L<start ( )|/"start ( )">,
L<pause ( )|/"pause ( )">

=item C<trackers>

Returns a list of all
L<Net::BitTorrent::Torrent::Tracker|Net::BitTorrent::Torrent::Tracker>
objects related to the torrent.

=item C<uploaded ( )>

Returns the total amount uploaded to remote peers since the client
started transferring data related to this .torrent.

See also: L<downloaded ( )|/"downloaded ( )">

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the  object's data structure.  If
called in void context, the structure is printed to C<STDERR>.
C<VERBOSE> is a boolean value.

=back

=head1 Events

When triggered, per-torrent callbacks receive two arguments: the
C<Net::BitTorrent::Torrent> object and a hashref containing pertinent
information.  Per-torrent callbacks also trigger client-wide callbacks
when the current torrent is queued.

Per-torrent callbacks are limited to tracker-, piece-, and file-related
events.  See L<Net::BitTorrent|Net::BitTorrent/"Events"> for client-wide
callbacks.

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2009 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the terms of The Artistic License 2.0.  See the F<LICENSE>
file included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered
by the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id: Torrent.pm 5476ff9 2009-09-07 04:37:45Z sanko@cpan.org $

=cut
