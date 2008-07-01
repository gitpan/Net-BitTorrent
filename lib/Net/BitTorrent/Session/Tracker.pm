package Net::BitTorrent::Session::Tracker;

# Honestly, this should be N::B::Session::Tracker::Tier;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN
            = q[$Id: Tracker.pm 23 2008-06-18 02:35:47Z sanko@cpan.org $];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 23 $)->numify / 1000;
    }
    use Net::BitTorrent::Util qw[min bdecode max compact shuffle :log];
    use Socket qw[SOL_SOCKET /TIMEO/ /F_INET/ SOCK_STREAM];
    use Fcntl qw[F_SETFL O_NONBLOCK];
    {
        my (%urls,                 %fileno,
            %socket,               %session,
            %next_announce,        %next_scrape,
            %connection_timestamp, %scrape_complete,
            %scrape_incomplete,    %scrape_downloaded,
            %connected,            %queue_outgoing,
            %queue_incoming
        );

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[session]}
                and $args->{q[session]}->isa(q[Net::BitTorrent::Session])
                and defined $args->{q[urls]})
            {   $self = bless \join(q[, ], @{$args->{q[urls]}}), $class;
                $urls{$self}                 = shuffle($args->{q[urls]});
                $session{$self}              = $args->{q[session]};
                $next_announce{$self}        = time;
                $next_scrape{$self}          = time;
                $connection_timestamp{$self} = 0;
                $scrape_complete{$self}      = 0;
                $scrape_incomplete{$self}    = 0;
                $scrape_downloaded{$self}    = 0;
                $connected{$self}            = 0;
                $session{$self}->get_client->_set_pulse($self, time);
            }
            return $self;
        }

        sub get_urls {
            die if $_[1];
            return $urls{$_[0]};
        }

        sub add_url {
            my ($self, $url) = @_;
            return push @{$urls{$_[@_]}}, $url;
        }

        sub remove_url {

            # XXX - TODO
        }

        sub _get_fileno {
            die if $_[1];
            return $fileno{$_[0]};
        }

        sub _get_socket {
            die if $_[1];
            return $socket{$_[0]};
        }

        sub get_session {
            die if $_[1];
            return $session{$_[0]};
        }

        sub get_client {
            die if $_[1];
            return $session{$_[0]}->get_client;
        }

        sub _get_connection_timestamp {
            die if $_[1];
            return $connection_timestamp{$_[0]};
        }

        sub _get_scrape_complete {
            die if $_[1];
            return $scrape_complete{$_[0]};
        }

        sub _get_scrape_incomplete {
            die if $_[1];
            return $scrape_incomplete{$_[0]};
        }

        sub _get_scrape_downloaded {
            die if $_[1];
            return $scrape_downloaded{$_[0]};
        }

        sub _get_connected {
            die if $_[1];
            return $connected{$_[0]};
        }

        sub _get_queue_outgoing {
            die if $_[1];
            return $queue_outgoing{$_[0]};
        }

        sub _get_queue_incoming {
            die if $_[1];
            return $queue_incoming{$_[0]};
        }

        sub _get_next_announce {
            die if $_[1];
            return $next_announce{$_[0]};
        }

        sub _get_next_scrape {
            die if $_[1];
            return $next_scrape{$_[0]};
        }

        sub _pulse {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            if (not defined $socket{$self}) {
                if ($next_announce{$self} <= time) {
                    $self->_announce;
                    $next_announce{$self} = time + 120;
                    $session{$self}
                        ->get_client->_set_pulse($self, time + 125);
                }
                elsif ($next_scrape{$self} <= time) {
                    $self->_scrape;
                    $next_scrape{$self} = time + 120;
                    $session{$self}->get_client->_set_pulse($self, time + 3);
                }
            }
            $session{$self}->get_client->_set_pulse($self,
                             min($next_announce{$self}, $next_scrape{$self}));
            return 1;
        }

        sub _disconnect {
            my ($self, $reason) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            close $socket{$self};
            $self->get_client->_do_callback(q[tracker_disconnect], $self);
            $session{$self}->get_client->_remove_connection($self);
            delete $socket{$self};
            delete $fileno{$self};
            $connected{$self}      = 0;
            $queue_outgoing{$self} = q[];
            $queue_incoming{$self} = q[];
            return 1;
        }

        sub _scrape {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $self->get_client->_do_callback(q[tracker_scrape], $self);
            if ($urls{$self}->[0] =~ m[^http:]) {
                my $infohash = $session{$self}->get_infohash;
                my $peer_id  = $session{$self}->get_client->get_peer_id;
                $infohash =~ s|(..)|\%$1|g;    # urlencode
                my %query_hash = (q[info_hash] => $infohash,
                                  q[peer_id]   => $peer_id);
                my $url
                    = $urls{$self}->[0]
                    . ($urls{$self}->[0] =~ m[\?] ? q[&] : q[?])
                    . join q[&], map { sprintf q[%s=%s], $_, $query_hash{$_} }
                    keys %query_hash;
                $url =~ s|/announce([^\/]*?)$|/scrape$1|;
                return $self->_tcp_connect($url);
            }
            elsif ($urls{$self}->[0] =~ m[^udp:]) {
                return $self->_udp_connect;
            }
            $self->get_client->_do_callback(q[tracker_error],
                                            q[Unsupported tracker]);
            return;
        }

        sub _announce {
            my ($self, $event) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            if ($urls{$self}->[0] =~ m[^http:]) {
                my $infohash = $session{$self}->get_infohash;
                my $peer_id  = $session{$self}->get_client->get_peer_id;
                $infohash =~ s|(..)|\%$1|g;    # urlencode
                my %query_hash = (
                    q[info_hash] => $infohash,
                    q[peer_id]   => $peer_id,
                    q[port]      => $session{$self}->get_client->get_sockport,
                    q[uploaded]  => $session{$self}->get_uploaded,
                    q[downloaded] => $session{$self}->get_downloaded,
                    q[left]       => (
                        $session{$self}->get_piece_size * scalar(
                            grep {
                                not $_->get_cached_integrity
                                    and $_->get_priority
                                } @{$session{$self}->get_pieces}
                        )
                    ),
                    q[key]        => $^T,
                    q[numwant]    => 200,
                    q[compact]    => 1,
                    q[no_peer_id] => 1,
                    (defined($event)
                     ? (q[event] => $event)
                     : ()
                    )
                );
                $self->_tcp_connect(
                          $urls{$self}->[0]
                        . ($urls{$self}->[0] =~ m[\?] ? q[&] : q[?])
                        . (
                        join q[&],
                        map {
                            sprintf q[%s=%s], $_, $query_hash{$_}
                            }
                            keys %query_hash
                        )
                );
            }
            elsif ($urls{$self}->[0] =~ m[^udp:]) {
                $self->_udp_connect;
            }
            else {
                $session{$self}->get_client->_do_callback(q[log], WARN,
                                                      q[Unsupported tracker]);
            }
            $self->get_client->_do_callback(q[tracker_announce], $self);
            return 1;
        }

        sub _tcp_connect {
            my ($self, $query) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my ($protocol, $host, undef, $port, $object)
                = $query =~ m{^([^:/]+)://([^/:]*)(:(\d+))?(/.*)$};
            my $resolve = gethostbyname($host);    # slow
            if (not defined $resolve or length $resolve != 4) {
                return;
            }
            $port = $port ? $port : 80;
            my $socket;
            if (not CORE::socket($socket,      &PF_INET,
                                 &SOCK_STREAM, getprotobyname(q[tcp]))
                )
            {   $self->get_client->_do_callback(q[tracker_error],
                                                q[Failed to create socket]);
            }
            elsif (not($^O eq q[MSWin32]
                       ? ioctl($socket, 0x8004667e, pack(q[I], 1))
                       : fcntl($socket, F_SETFL, O_NONBLOCK)
                   )
                )
            {   $self->get_client->_do_callback(q[tracker_error],
                                     q[Failed to set socket to non-blocking]);
            }
            elsif (not setsockopt($socket, SOL_SOCKET,
                                  SO_SNDTIMEO, pack('LL', 15, 0))
                   or not setsockopt($socket, SOL_SOCKET,
                                     SO_RCVTIMEO, pack('LL', 15, 0))
                )
            {   $self->get_client->_do_callback(q[tracker_error],
                                  q[Failed to set socket connection timeout]);
            }
            elsif (
                  not
                  connect($socket, pack(q[Sna4x8], &AF_INET, $port, $resolve))
                  and $^E
                  and ($^E != 10036)
                  and ($^E != 10035))
            {   $self->get_client->_do_callback(q[tracker_error],
                                        sprintf q[Failed to connect: %s (%d)],
                                        $^E, $^E + 0);
            }
            else {
                $socket{$self}               = $socket;
                $fileno{$self}               = fileno($socket{$self});
                $connection_timestamp{$self} = time;
                $queue_outgoing{$self}
                    = join(qq[\015\012],
                           qq[GET $object HTTP/1.0],
                           q[Connection: close],
                           qq[Host: $host:$port],
                           q[Accept: text/plain],
                           q[Accept-Encoding:],
                           qq[User-Agent: Net::BitTorrent/]
                               . $Net::BitTorrent::VERSION,
                           q[],
                           q[]);
                return $session{$self}->get_client->_add_connection($self);
            }
            return;
        }

        sub _tcp_parse_data {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my ($head, $body) = split m[\015?\012\015?\012],
                $queue_incoming{$self}, 2;
            if ($head and not $body) {
                $body = $head;
                $head = q[HTTP/1.0 200 OK];
            }    # bad server
            my %headers = map {
                my ($k, $v) = split(m[[^\w-]+], $_, 2);
                $k => $v
                }
                split(m[\015?\012], $head);
            if ((defined($headers{q[Content-Length]})
                 and (length($body) == $headers{q[Content-Length]})
                )
                or (length($body))
                )
            {   my $decoded_data = bdecode($body);
                if (defined $decoded_data) {
                    if (defined $decoded_data->{q[failure reason]}) {
                        $self->get_client->_do_callback(q[tracker_error],
                                   $self, $decoded_data->{q[failure reason]});
                    }
                    elsif (defined $decoded_data->{q[files]}) {
                        my $file_hash = $decoded_data->{q[files]}{pack q[H*],
                            $session{$self}->get_infohash};
                        $scrape_complete{$self} = $file_hash->{q[complete]};
                        $scrape_downloaded{$self}
                            = $file_hash->{q[downloaded]};
                        $scrape_incomplete{$self}
                            = $file_hash->{q[incomplete]};
                        $next_scrape{$self}
                            = max((defined $decoded_data->{q[flags]}
                                       {q[min_request_interval]}
                                   ? $decoded_data->{q[flags]}
                                       {q[min_request_interval]}
                                   : 0
                                  ),
                                  900
                            ) + time;
                        $self->get_client->_do_callback(
                                                       q[tracker_scrape_okay],
                                                       $self);
                    }
                    else {
                        if (ref $decoded_data->{q[peers]} eq q[ARRAY])
                        {    # Tracker is old and doesn't listen. Handed us
                                # non-compacted peer list
                            $decoded_data->{q[peers]}
                                = compact($decoded_data->{q[peers]});
                        }
                        $session{$self}
                            ->append_nodes($decoded_data->{q[peers]});
                        $next_announce{$self}
                            = max((defined $decoded_data->{q[interval]}
                                   ? $decoded_data->{q[interval]}
                                   : 1800
                                  ),
                                  (defined $decoded_data->{q[min interval]}
                                   ? $decoded_data->{q[min interval]}
                                   : 0
                                  )
                            ) + time;
                        $self->get_client->_do_callback(
                                                     q[tracker_announce_okay],
                                                     $self);
                    }
                }
                return $self->_disconnect;
            }
            return;
        }

        sub _udp_connect {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $self->get_client->_do_callback(q[tracker_error], $self,
                                            q[UDP trackers are unsupported.]);
            $next_announce{$self} = time + (60 * 15);    # XXX - remove!
            return 0;
        }

        sub _udp_write {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $self->get_client->_do_callback(q[tracker_error], $self,
                                            q[UDP trackers are unsupported.]);
            return 0;
        }

        sub _udp_read {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $self->get_client->_do_callback(q[tracker_error],
                                            q[UDP trackers are unsupported.]);
            return 0;
        }

        sub _udp_disconnect {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $self->get_client->_do_callback(q[tracker_error], $self,
                                            q[UDP trackers are unsupported.]);
            return 0;
        }

        sub _udp_parse_data {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $self->get_client->_do_callback(q[tracker_error], $self,
                                            q[UDP trackers are unsupported.]);
            return 0;
        }

        sub _process_one {
            my ($self, $read, $write) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my ($actual_read, $actual_write) = (0, 0);
            if ($write and defined $socket{$self}) {
                $actual_write =
                    syswrite($socket{$self},
                             substr($queue_outgoing{$self}, 0, $write, q[]),
                             $write);
                if ($actual_write) {
                    $session{$self}
                        ->get_client->_do_callback(q[tracker_outgoing_data],
                                                   $self, $actual_write);
                }
                else { $self->_disconnect; goto RETURN; }
            }
            if ($read and defined $socket{$self}) {
                $actual_read =
                    sysread($socket{$self},
                            $queue_incoming{$self},
                            $read,
                            (defined $queue_incoming{$self}
                             ? length($queue_incoming{$self})
                             : 0
                            )
                    );
                if ($actual_read) {
                    if (not $connected{$self}) {
                        $connected{$self} = 1;
                        $session{$self}
                            ->get_client->_do_callback(q[tracker_connect],
                                                       $self);
                    }
                    $session{$self}
                        ->get_client->_do_callback(q[tracker_incoming_data],
                                                   $self, $actual_read);
                    $self->_parse_packet;
                }
                else {
                    $self->_disconnect();
                }
            }
        RETURN:
            return ($actual_read, $actual_write);
        }

        sub _parse_packet {
            my ($self) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            if ($urls{$self}->[0] =~ m[^http:]) {
                $self->_tcp_parse_data;
            }
            elsif ($urls{$self}->[0] =~ m[^udp:]) {
                $self->_udp_parse_data;
            }
            else {
                $session{$self}->get_client->_do_callback(q[log], WARN,
                                                      q[Somethin' is wrong!]);
            }
            return;
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my @values = (
                        $urls{$self}->[0],
                        (q[=] x (27 + length($urls{$self}->[0]))),
                        ($scrape_complete{$self} + $scrape_incomplete{$self}),
                        $scrape_complete{$self},
                        $scrape_incomplete{$self},
                        $scrape_downloaded{$self},
                        $next_scrape{$self} - time,
                        $next_announce{$self} - time,
                        $session{$self}->get_client->_get_pulse($self) - time,
            );
            $_ = (sprintf q[%dm %ss%s],
                  int(abs($_) / 60),
                  abs($_) % 60,
                  $_ > 0 ? q[] : q[ ago]
            ) for @values[6 .. 8];
            my $dump = sprintf( <<'END', @values);
Net::BitTorrent::Session::Tracker (%s)
%s
Basic Information:
  Total peers:     %d
  Complete:        %d
  Incomplete:      %d
  Total Downloads: %d
  Next scrape:     %s
  Next announce:   %s
  Next pulse:      %s
END
            if ($advanced) {
                my @adv_values = (scalar(@{$urls{$self}}),
                                  join(qq[\n    ], @{$urls{$self}})
                );
                $dump .= sprintf( <<'END', @adv_values);

Advanced Information:
  URL list: (%d)
     %s
END
            }
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my $self = shift;
            $session{$self}->get_client->_del_pulse($self)
                if defined $session{$self}
                    and defined $session{$self}->get_client;
            delete $urls{$self};
            delete $socket{$self};
            delete $session{$self};
            delete $fileno{$self};
            delete $next_announce{$self};
            delete $next_scrape{$self};
            delete $connection_timestamp{$self};
            delete $scrape_complete{$self};
            delete $scrape_incomplete{$self};
            delete $scrape_downloaded{$self};
            delete $connected{$self};
            delete $queue_outgoing{$self};
            delete $queue_incoming{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Tracker - Single Tier of BitTorrent Trackers

=head1 Constructor

=over 4

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::Session::Tracker> object.  This
constructor should not be used directly.

See also:
L<Net::BitTorrent::Session|Net::BitTorrent::Session/"add_tracker ( URLS )">

=back

=head1 Methods

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Tracker> object's data structure.  If called
in void context, the structure is printed to C<STDERR>.

See also:
L<Net::BitTorrent|Net::BitTorrent/"as_string ( [ VERBOSE ] )">

=item C<get_client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
tracker.

=item C<get_session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this request.

=item C<get_urls ( )>

Returns the list of URLs contained in this tier.

See Also: L<add_url( )|/"add_url ( URL )">,
L<remove_url( )|/"remove_url ( URL )">

=item C<add_url ( URL )>

Adds a new tracker url to this tier.

See Also: L<remove_url( )|/"remove_url ( URL )">,
L<get_urls( )|/"get_urls ( )">

=item C<remove_url ( URL )>

Removes a tracker from the list in this tier.

I<This method is a NOOP.  See the next release...>

See Also: L<add_url( )|/"add_url ( URL )">,
L<get_urls( )|/"get_urls ( )">

=back

=head1 BUGS/TODO

=over 4

=item *

Does not support UDP or HTTPS trackers.

=item *

While we don't hammer the trackers, the current version of this module
does not comply with the current draft of the Multitracker Metadata
Extension specification's order of processing.

See also: http://www.bittorrent.org/beps/bep_0012.html

=item *

We do not send stop and complete announcements when required.  This
is actually a L<Net::BitTorrent::Session|Net::BitTorrent::Session> and
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer> bug.

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl 5.10 (or higher).  See
http://www.perl.com/perl/misc/Artistic.html or the F<LICENSE> file
included with this distribution.

All POD documentation is covered by the Creative Commons Attribution-
Noncommercial-Share Alike 3.0 License
(http://creativecommons.org/licenses/by-nc-sa/3.0/us/).

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id: Tracker.pm 23 2008-06-18 02:35:47Z sanko@cpan.org $

=cut
