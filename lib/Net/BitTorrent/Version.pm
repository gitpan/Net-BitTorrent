package Net::BitTorrent::Version;
{
    use strict;
    use warnings;

    #
    use version qw[qv];    # core as of 5.009
    our $SVN = q[$Id$];
    our $VERSION_BASE = 27; our $UNSTABLE_RELEASE = 4; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new(($VERSION_BASE))->numify / 1000), $UNSTABLE_RELEASE);
    our $PRODUCT_TOKEN = qq[Net::BitTorrent/$VERSION ($^O)];    # ext protocol

    sub gen_peerid { #
        return pack(
            q[a20],
            (sprintf(
                 q[NB%03d%1s-%8s%5s],
                 (q[$Rev: 28 $] =~ m[(\d+)]g),
                 ($UNSTABLE_RELEASE ? q[S] : q[C]),
                 (join q[],
                  map {
                      [q[A] .. q[Z], q[a] .. q[z], 0 .. 9, qw[- . _ ~]]
                      ->[rand(66)]
                      } 1 .. 8
                 ),
                 q[Sanko],
             )
            )
        );
    }
    1;
}

=head1 NAME

Net::BitTorrent::Version - The Net::BitTorrent project-wide version numbers

=head1 DESCRIPTION

Because of the problems coordinationg revision numbers in a distributed
version control system and across a directory full of Perl modules, this
module provides a central location for the project's overal release
number, the version string provided in Extended Protocol handshakes,
and the Peer ID generator.

=head1 Peer ID Specification

This section describes and provides examples of the Peer ID format used
by the current release of the C<Net::BitTorrent> module.

=head3 Format

This non-standard format was developed to be URL-safe, unique to the
implementation, and "human parsable."

There are three distinct sections to the Peer IDs generated: the
L<header|/Header>, the L<mid-section|/Mid-section>, and the
L<signature|/Signature>.  Consider this example:

 NB004S-rogzGB1v--SVN

Here, C<NB004S> is the header, C<-rogzGB1v> is the mid-section, and
C<--SVN> is the trailing signature.

=head4 Header

Two uppercase characters ('C<NB>') followed by three digits representing
the SVN revision number with leading zeros, a single character
potentially indicating stability and how the release was obtained, and a
single hyphen (C<->).

If the client is a CPAN build, the sixth byte is the capital letter
'C<C>'.  If the client is running a version checked out from public SVN
(considered unstable), the sixth byte is the capital letter 'S'.  Any
other characters in the sixth byte are unsupported and may indicate a bad
client.

=head4 Mid-section

Following that are eight random characters in the following range:

 ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~

That is, all uppercase and lowercase letters, decimal digits, as well as
the hyphen, period, underscore, and tilde (66 total).  These are all
characters allowed in a URL without encoded (referred to as "Unreserved
Characters in [rfc://3986]) to reduce weight on the tracker during
announce.  On a popular tracker, every bit (and byte) helps.

This section is inspired by the pseudo-base64 set used by SHADOW's
BitTornado client.

=head4 Signature

The final five characters may be random or static and should not be used
in identifying the version of the client software.  Some early versions
even have my name in this spot.  .:shrugs:.

=head3 CPAN Version Numbers

Stable CPAN releases will have the sixth byte set to 'C<C>' (capital
letter 'c').

Unstable releases (referred to as DEVELOPER releases on PAUSE) on CPAN
will have this bit set as if the user checked the source out from SVN,
that is, the sixth byte will be 'C<S>' (capital letter 's').  These will
be released on CPAN with version numbers matching C<m[\d\.\d+_\d]>.  See
the PAUSE FAQ section entitled "Developer Releases"
(L<http://www.cpan.org/modules/04pause.html>).

Version numbers will be some value less than one with the revision number
in the three significant decimal places.  Eventually, I would like to
make a v1.0 release of Net::BitTorrent on CPAN.  The information in this
document and the means of generating the module's version number will
need to reflect that.

=head3 Examples

=over

=item C<NB393C-04.9EraQ--SVN>

This would be the stable CPAN release C<v0.393>.  The C<--SVN> signature
should be ignored.

=item C<NB003X-9E-ayR6b-BTE<lt>3>

Improper Peer ID; the sixth bit is neither 'C' nor 'S'.  Possibly fake.

=item C<NB065S--09Egae69sy8W>

Completely legal Peer ID generated by SVN/Dev r65.

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

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

=for svn $Id$

=cut
