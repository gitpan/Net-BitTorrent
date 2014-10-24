package Net::BitTorrent::Network::Utility;
{
    use strict;
    use warnings;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 2; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use AnyEvent;
    use AnyEvent::Socket qw[];
    use Socket qw[/SOCK_/ /F_INET/ inet_aton /sockaddr_in/ inet_ntoa
        SOL_SOCKET SO_REUSEADDR
    ];
    my %cache;
    require Exporter;
    our @ISA = qw[Exporter];
    our %EXPORT_TAGS = (socket   => [qw[client server]],
                        paddr    => [qw[sockaddr paddr2ip]],
                        sockaddr => [qw[pack_sockaddr unpack_sockaddr]],
                        vars     => [qw[%cache]]
    );
    our @EXPORT_OK = @{$EXPORT_TAGS{'all'}}
        = sort map {@$_} values %EXPORT_TAGS;

    sub sockaddr ($$) {
        my $done = 0;
        my $return;
        AnyEvent::Socket::resolve_sockaddr(
            $_[0],
            $_[1],
            0, undef, undef,
            sub {
                $return = $_[0]->[3];
                $done++;
            }
        );
        AnyEvent->one_event while !$done;
        return $return;
    }

    sub paddr2ip ($) {    # snagged from NetAddr::IP::Util
        return inet_ntoa($_[0]) if length $_[0] == 4;    # ipv4
        return unless length($_[0]) == 16;
        my @hex = (unpack('n8', $_[0]));
        $hex[9] = $hex[7] & 0xff;
        $hex[8] = $hex[7] >> 8;
        $hex[7] = $hex[6] & 0xff;
        $hex[6] >>= 8;
        my $return = sprintf("%X:%X:%X:%X:%X:%X:%D:%D:%D:%D", @hex);
        $return =~ s/(0+:)+/:/;
        $return =~ s/^0+//;
        $return =~ s/^:+/::/;
        $return =~ s/::0+/::/;
        return $return;
    }

    sub pack_sockaddr {
        my ($port, $packed_host) = @_;
        my $return
            = length $packed_host == 4
            ? sockaddr_in($port, $packed_host)
            : pack('SnLa16L', PF_INET6, $port, 0, $packed_host, 0);
        return $return;
    }

    sub unpack_sockaddr {
        my ($packed_host) = @_;
        return
            length $packed_host == 28
            ? (unpack('SnLa16L', $packed_host))[1, 3]
            : unpack_sockaddr_in($packed_host);
    }

    sub client {
        my ($host, $port, $ready, $prepare) = @_;
        &AnyEvent::Socket::tcp_connect;
    }

    sub server {
        my ($host, $port, $callback, $prepare, $proto) = @_;
        my $sockaddr = sockaddr($host, $port);
        my $type = length $sockaddr == 16 ? PF_INET : PF_INET6;
        socket my ($socket), $type,
            $proto eq 'udp' ? SOCK_DGRAM : SOCK_STREAM, getprotobyname($proto)
            || return;

        # - What is the difference between SO_REUSEADDR and SO_REUSEPORT?
        #    [http://www.unixguide.net/network/socketfaq/4.11.shtml]
        # - setsockopt - what are the options for ActivePerl under Windows NT?
        #    [http://perlmonks.org/?node_id=63280]
        #      setsockopt($_tcp, SOL_SOCKET, SO_REUSEADDR, pack(q[l], 1))
        #         or return;
        # SO_REUSEPORT is undefined on Win32... Boo...
        #return
        #    if !setsockopt $socket, SOL_SOCKET, SO_REUSEADDR, pack('l', 1);
        return if !bind $socket, $sockaddr;
        if (defined $prepare) {
            my ($_port, $packed_ip) = unpack_sockaddr getsockname $socket;
            $prepare->($socket, paddr2ip($packed_ip), $_port);
        }
        require AnyEvent::Util;
        AnyEvent::Util::fh_nonblocking $socket, 1;
        return if $proto ne 'udp' && !listen($socket, 8);
        return AE::io(
            $socket, 0,
            $proto eq 'udp'
            ? sub {
                my $flags = 0;
                if ($socket
                    && (my $peer = recv $socket, my ($data), 16 * 1024,
                        $flags))
                {   my ($service, $host) = unpack_sockaddr $peer;
                    $callback->($socket, $peer, paddr2ip($host), $service,
                                $data, $flags
                    );
                }
                }
            : sub {
                while ($socket
                       && (my $peer = accept my ($fh), $socket))
                {   my ($service, $host) = unpack_sockaddr $peer;
                    $callback->($fh, $peer, paddr2ip($host), $service);
                }
            }
        );
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Network::Utility - General networking utility functions

=head1 Description

Nothing to see here.

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

=for rcs $Id: Notes.pod 5d9d1db 2010-06-07 13:52:01Z sanko@cpan.org $

=cut
