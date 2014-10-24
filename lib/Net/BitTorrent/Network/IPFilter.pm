package Net::BitTorrent::Network::IPFilter;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use 5.012.000;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 6; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../';
    use Net::BitTorrent::Network::Utility qw[:paddr];
    sub BUILD { 1; }
    has 'rules' => (
          isa => 'ArrayRef[Net::BitTorrent::Network::IPFilter::Rule]' => is =>
              'ro',
          traits   => ['Array'],
          coerce   => 1,
          init_arg => undef,
          default  => sub { [] },
          handles  => {
                      add_rule            => 'push',
                      count_rules         => 'count',
                      is_empty            => 'is_empty',
                      get_rule            => 'get',
                      first_rule          => 'first',
                      grep_rules          => 'grep',
                      map_rules           => 'map',
                      sort_rules          => 'sort',
                      sort_rules_in_place => 'sort_in_place',
                      shuffle_rules       => 'shuffle',
                      clear_rules         => 'clear',
                      insert_rule         => 'insert',
                      delete_rule         => 'delete',
                      push_rule           => 'push',
                      pop_rule            => 'pop'
          }
    );
    around 'add_rule' => sub {
        my ($c, $s, $l, $u, $a, $d) = @_;
        $l = blessed $l? $l : sub {
            require Net::BitTorrent::Network::IPFilter::Rule;
            Net::BitTorrent::Network::IPFilter::Rule->new(lower        => $l,
                                                          upper        => $u,
                                                          access_level => $a,
                                                          description  => $d
            );
            }
            ->();
        return $c->($s, $l) ? $l : ();
    };

    sub load {
        my ($s, $path) = @_;
        open(my $IPFilter, '<', $path) || return;
        for my $line (<$IPFilter>) {
            next if $line =~ m[(?:^#|^$)];
            my ($range, $access_level, $desc)
                = ($line =~ m[^(.+-.+)\s*,\s*(\d+)\s*,\s*(.+)\s*$]);
            next if !$range;
            my ($start, $end) = ($range =~ m[^(.+)\s*-\s*(.+)\s*$]);
            $_ =~ s[\s][]g for $start, $end;
            $s->add_rule($start, $end, $access_level, $desc);
        }
        1;
    }

    sub save {
        my ($s, $path) = @_;
        open(my $IPFilter, '>', $path) || return;
        for my $rule (
            $s->sort_rules(
                sub {
                    $_[0]->lower cmp $_[1]->lower
                        || $_[0]->upper cmp $_[1]->upper;
                }
            )
            )
        {   syswrite $IPFilter, $rule->_as_string . "\n";
        }
        return close $IPFilter;
    }

    sub is_banned {
        my ($s, $ip) = @_;
        return $s->first_rule(
            sub {
                $_->in_range($ip) && $_->access_level < 127;
            }
        ) || ();
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Network::IPFilter - Simple, rule-based IP filter

=head1 Description

    # Example of a "ipfilter.dat" file
    #
    # All entered IP ranges will be blocked in both directions. Be careful
    # what you enter here. Wrong entries may totally block access to the
    # network.
    #
    # Format:
    # IP-Range , Access Level , Description
    #
    # Access Levels:
    # 127 blocked
    # >=127 permitted

    064.094.089.000 - 064.094.089.255 , 000 , Gator.com

This entry will block the IPs from 064.094.089.000 to 064.094.089.255, i.e.
L<Net::BitTorrent> will not connect to any IP in this range.

Warning:

The example above will block any connection to the specified IP-range. This
may reduce the number of sources for downloads.

At the moment only one access levels are implemented. A value below 127 means
that any connection-attempt is denied.

=head2 Notes

Remember...

=over

=item * Filtering will prevent up and downloading to the banned clients

=item * Filtering is done at the protocol level so no connection to banned
clients is ever established

=back

=head1 my $filter = Net::BitTorrent::Network::IPFilter->B<new>( )

This constructs a new, empty object. There are currently no accepted
arguments.

=head1 $filter->B<add_rule>( $rule )

This method adds a new L<range|Net::BitTorrent::Network::IPFilter::Rule> to
the in-memory ipfilter.

=head1 $filter->B<add_rule>( $lower, $upper, $access_level, $description )

This method coerces the arguments into a new
L<range|Net::BitTorrent::Network::IPFilter::Rule> which is the added to the
in-memory ipfilter.

=head1 $filter->B<count_rules>( )

Returns how many L<rule|Net::BitTorrent::Network::IPFilter::Rule>s are loaded.

=head1 $filter->B<is_empty>( )

Returns a boolean value indicating whether or not there are any
L<rule|Net::BitTorrent::Network::IPFilter::Rule>s loaded in the ipfilter.

=head1 $filter->B<clear_rules>( )

Deletes all L<rule|Net::BitTorrent::Network::IPFilter::Rule>s from the
ipfilter.

=head1 $filter->B<load>( $path )

Slurps an ipfilter.dat-like file and adds the
L<rule|Net::BitTorrent::Network::IPFilter::Rule>s found inside to the
ipfilter.

=head1 $filter->B<save>( $path )

Stores the in-memory ipfilter to disk.

=head1 $filter->B<is_banned>( $ip )

Indicates whether or not C<$ip> is banned. If so, the
L<rule|Net::BitTorrent::Network::IPFilter::Rule> in which it was found is
returned. If not, a false value is returned.

=head1 IPv6 Support

The standard ipfilter.dat only supports IPv4 addresses but
L<Net::BitTorrent>'s current implementation supports IPv6 as well. Keep this
in mind when L<storing|/save> an ipfilter.dat file to disk.

=head1 See Also

L<Emule Project's ipfilter.dat documentation|http://www.emule-project.net/home/perl/help.cgi?l=1&topic_id=142&rm=show_topic>

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

=for rcs $Id: IPFilter.pm cf06f07 2010-07-05 15:33:02Z sanko@cpan.org $

=cut
