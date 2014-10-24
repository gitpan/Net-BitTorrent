package Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use Carp qw[carp];
    use List::Util qw[shuffle];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../..';
    use Net::BitTorrent::Types qw[:tracker];
    has 'url' => (isa      => subtype(as Str => where {m[^udp://.+]}),
                  is       => 'ro',
                  required => 1,
    );

    sub scrape {
        my ($self) = @_;
        warn 'UDP scrape!';
    }

    sub announce {
        my ($self, $event) = @_;
        warn 'UDP announce!';
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Tracker::UDP - A single UDP-based tracker

=head1 See Also

=over

=item BEP 15:

http://bittorrent.org/beps/bep_0015.html

=back

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

=for rcs $Id: UDP.pm a7f61f8 2010-06-27 02:13:37Z sanko@cpan.org $

=cut
