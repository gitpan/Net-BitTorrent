package Net::BitTorrent::Storage::File;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    extends 'Net::BitTorrent::Storage::Node';
    has 'index' => (is       => 'ro',
                    isa      => 'Maybe[Int]',
                    required => 1
    );
    has 'length' => (is       => 'ro',
                     isa      => 'Int',
                     required => 1
    );
    has 'offset' => (is      => 'ro',
                     isa     => 'Int',
                     default => 0
    );
    has 'priority' => (is      => 'rw',
                       isa     => subtype(as 'Int' => as enum([0 .. 3])),
                       default => 2
    );
    around 'read' => sub ($;$$) {
        my ($code, $self, $offset, $length) = @_;
        $offset //= 0;
        $length //= $self->length - $offset;
        return if $length + $offset > $self->length;
        return $code->($self, $offset, $length);
    };
    around 'write' => sub ($$$) {
        my ($code, $self, $offset, $data) = @_;
        $offset //= 0;
        return if length($data) + $offset > $self->length;
        return $code->($self, $offset, $data);
    };
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

=for rcs $Id: File.pm a7f61f8 2010-06-27 02:13:37Z sanko@cpan.org $

=cut
