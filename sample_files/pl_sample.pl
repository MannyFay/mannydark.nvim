#!/usr/bin/env perl
# -*- coding: utf-8 -*-

=head1 NAME

sample.pl - Comprehensive Perl language sample demonstrating all syntax features

=head1 SYNOPSIS

    perl sample.pl [options]

=head1 DESCRIPTION

This script demonstrates Perl's syntax features including:
- Object-oriented programming
- Regular expressions
- References and data structures
- Subroutines and closures
- File I/O
- And much more...

=head1 AUTHOR

Sample Author

=head1 VERSION

1.0.0

=cut

use strict;
use warnings;
use feature qw(say state signatures);
use utf8;
use open ':std', ':encoding(UTF-8)';

use Carp qw(croak confess);
use Data::Dumper;
use Scalar::Util qw(blessed reftype looks_like_number);
use List::Util qw(sum max min reduce first any all none);
use File::Basename;
use File::Spec;
use Getopt::Long;
use Pod::Usage;
use JSON;
use Time::HiRes qw(time sleep);
use Try::Tiny;

# ============================================================================
# Constants
# ============================================================================

use constant {
    MAX_BUFFER_SIZE => 1024,
    PI              => 3.14159265358979323846,
    GREETING        => 'Hello, Perl!',
    TRUE            => 1,
    FALSE           => 0,
};

# Readonly alternative
our $VERSION = '1.0.0';
my $AUTHOR = 'Sample Author';

# ============================================================================
# Package/Class Definition
# ============================================================================

package Point {
    use strict;
    use warnings;
    use overload
        '+'  => \&add,
        '-'  => \&subtract,
        '*'  => \&multiply,
        '""' => \&stringify,
        '<=>' => \&compare;

    sub new {
        my ($class, %args) = @_;
        my $self = {
            x => $args{x} // 0,
            y => $args{y} // 0,
            z => $args{z} // 0,
        };
        return bless $self, $class;
    }

    sub x { shift->{x} }
    sub y { shift->{y} }
    sub z { shift->{z} }

    sub set_x { my ($self, $val) = @_; $self->{x} = $val; }
    sub set_y { my ($self, $val) = @_; $self->{y} = $val; }
    sub set_z { my ($self, $val) = @_; $self->{z} = $val; }

    sub distance {
        my ($self, $other) = @_;
        my $dx = $self->{x} - $other->{x};
        my $dy = $self->{y} - $other->{y};
        my $dz = $self->{z} - $other->{z};
        return sqrt($dx**2 + $dy**2 + $dz**2);
    }

    sub add {
        my ($self, $other) = @_;
        return Point->new(
            x => $self->{x} + $other->{x},
            y => $self->{y} + $other->{y},
            z => $self->{z} + $other->{z},
        );
    }

    sub subtract {
        my ($self, $other) = @_;
        return Point->new(
            x => $self->{x} - $other->{x},
            y => $self->{y} - $other->{y},
            z => $self->{z} - $other->{z},
        );
    }

    sub multiply {
        my ($self, $scalar) = @_;
        return Point->new(
            x => $self->{x} * $scalar,
            y => $self->{y} * $scalar,
            z => $self->{z} * $scalar,
        );
    }

    sub stringify {
        my $self = shift;
        return sprintf("Point(%g, %g, %g)", $self->{x}, $self->{y}, $self->{z});
    }

    sub compare {
        my ($self, $other) = @_;
        my $origin = Point->new();
        return $self->distance($origin) <=> $other->distance($origin);
    }

    sub origin {
        return Point->new(x => 0, y => 0, z => 0);
    }
}

package Person {
    use strict;
    use warnings;

    sub new {
        my ($class, %args) = @_;
        my $self = {
            name  => $args{name} // 'Unknown',
            age   => $args{age} // 0,
            email => $args{email},
        };
        return bless $self, $class;
    }

    sub name  { shift->{name} }
    sub age   { shift->{age} }
    sub email { shift->{email} }

    sub set_name  { my ($self, $val) = @_; $self->{name} = $val; }
    sub set_age   { my ($self, $val) = @_; $self->{age} = $val; }
    sub set_email { my ($self, $val) = @_; $self->{email} = $val; }

    sub to_string {
        my $self = shift;
        return sprintf("%s (%d)", $self->{name}, $self->{age});
    }
}

# ============================================================================
# Abstract Base Class
# ============================================================================

package Shape {
    use strict;
    use warnings;
    use Carp qw(croak);

    sub new {
        my ($class, %args) = @_;
        my $self = {
            name  => $args{name} // 'Shape',
            color => $args{color} // 'black',
        };
        return bless $self, $class;
    }

    sub name  { shift->{name} }
    sub color { shift->{color} }

    sub set_color { my ($self, $val) = @_; $self->{color} = $val; }

    sub area {
        croak "Subclass must implement area()";
    }

    sub perimeter {
        croak "Subclass must implement perimeter()";
    }

    sub describe {
        my $self = shift;
        return sprintf("%s: area=%.2f, perimeter=%.2f",
            $self->{name}, $self->area(), $self->perimeter());
    }

    sub draw {
        my $self = shift;
        say "Drawing " . $self->{name} . " in " . $self->{color};
    }
}

package Circle {
    use strict;
    use warnings;
    use parent -norequire, 'Shape';

    sub new {
        my ($class, %args) = @_;
        my $self = $class->SUPER::new(name => 'Circle', %args);
        $self->{radius} = $args{radius} // 1;
        return $self;
    }

    sub radius { shift->{radius} }
    sub set_radius { my ($self, $val) = @_; $self->{radius} = $val; }

    sub area {
        my $self = shift;
        return main::PI * $self->{radius} ** 2;
    }

    sub perimeter {
        my $self = shift;
        return 2 * main::PI * $self->{radius};
    }

    sub draw {
        my $self = shift;
        $self->SUPER::draw();
        say "  radius: " . $self->{radius};
    }
}

package Rectangle {
    use strict;
    use warnings;
    use parent -norequire, 'Shape';

    sub new {
        my ($class, %args) = @_;
        my $self = $class->SUPER::new(name => 'Rectangle', %args);
        $self->{width}  = $args{width} // 1;
        $self->{height} = $args{height} // 1;
        return $self;
    }

    sub width  { shift->{width} }
    sub height { shift->{height} }

    sub set_width  { my ($self, $val) = @_; $self->{width} = $val; }
    sub set_height { my ($self, $val) = @_; $self->{height} = $val; }

    sub area {
        my $self = shift;
        return $self->{width} * $self->{height};
    }

    sub perimeter {
        my $self = shift;
        return 2 * ($self->{width} + $self->{height});
    }
}

# ============================================================================
# Back to Main Package
# ============================================================================

package main;

# ============================================================================
# Subroutines
# ============================================================================

sub add ($a, $b) {
    return $a + $b;
}

sub greet {
    my ($name, $greeting) = @_;
    $greeting //= 'Hello';
    return "$greeting, $name!";
}

sub sum_all {
    my @numbers = @_;
    return sum(@numbers) // 0;
}

sub process {
    my %args = @_;
    return { map { $_ => $args{$_} } grep { defined $args{$_} } keys %args };
}

# Recursive subroutine
sub factorial {
    my $n = shift;
    return 1 if $n <= 1;
    return $n * factorial($n - 1);
}

# Tail recursive with goto
sub factorial_tail {
    my ($n, $acc) = @_;
    $acc //= 1;
    return $acc if $n <= 1;
    @_ = ($n - 1, $n * $acc);
    goto &factorial_tail;
}

# State variable
sub counter {
    state $count = 0;
    return ++$count;
}

# Closure
sub make_multiplier {
    my $factor = shift;
    return sub {
        my $x = shift;
        return $x * $factor;
    };
}

# Higher-order function
sub apply_twice {
    my ($func, $value) = @_;
    return $func->($func->($value));
}

# Callback pattern
sub with_timing {
    my $code = shift;
    my $start = time();
    my $result = $code->();
    my $elapsed = time() - $start;
    printf "Elapsed: %.4f seconds\n", $elapsed;
    return $result;
}

# ============================================================================
# Regular Expressions
# ============================================================================

sub regex_examples {
    my $text = 'Hello, my email is user@example.com and phone is 555-1234';

    # Basic match
    if ($text =~ /email/) {
        say "Found 'email'";
    }

    # Capture groups
    if ($text =~ /email is (\S+)/) {
        my $email = $1;
        say "Email: $email";
    }

    # Named captures
    if ($text =~ /(?<email>\S+@\S+)/) {
        say "Email: $+{email}";
    }

    # Global match
    my @words = $text =~ /(\w+)/g;
    say "Words: @words";

    # Substitution
    my $replaced = $text;
    $replaced =~ s/\d+/X/g;
    say "Replaced: $replaced";

    # Transliteration
    my $lower = $text;
    $lower =~ tr/A-Z/a-z/;

    # Split
    my @parts = split /[,\s]+/, $text;

    # Case-insensitive
    if ($text =~ /hello/i) {
        say "Found hello (case-insensitive)";
    }

    # Multi-line
    my $multiline = "Line1\nLine2\nLine3";
    my @lines = $multiline =~ /^(.+)$/mg;

    # Extended syntax
    if ($text =~ /
        (\w+)       # Username
        @           # At symbol
        (\w+)       # Domain
        \.          # Dot
        (\w+)       # TLD
    /x) {
        say "Matched email pattern";
    }

    return 1;
}

# ============================================================================
# Data Structures
# ============================================================================

sub data_structure_examples {
    # Arrays
    my @array = (1, 2, 3, 4, 5);
    push @array, 6;
    pop @array;
    unshift @array, 0;
    shift @array;
    my $len = scalar @array;
    my @slice = @array[1..3];

    # Hashes
    my %hash = (
        one   => 1,
        two   => 2,
        three => 3,
    );
    $hash{four} = 4;
    delete $hash{four};
    my @keys = keys %hash;
    my @values = values %hash;
    my $exists = exists $hash{one};

    # References
    my $array_ref = \@array;
    my $hash_ref = \%hash;
    my $scalar_ref = \42;
    my $code_ref = sub { say "Hello" };

    # Anonymous references
    my $anon_array = [1, 2, 3];
    my $anon_hash = { a => 1, b => 2 };

    # Dereferencing
    my @arr = @{$array_ref};
    my %h = %{$hash_ref};
    my $val = ${$scalar_ref};
    $code_ref->();

    # Arrow notation
    my $first = $array_ref->[0];
    my $value = $hash_ref->{one};

    # Complex structures
    my $complex = {
        name => 'Test',
        items => [1, 2, 3],
        nested => {
            deep => 'value',
            array => [
                { id => 1 },
                { id => 2 },
            ],
        },
    };

    # Access deep structure
    my $deep = $complex->{nested}{deep};
    my $id = $complex->{nested}{array}[0]{id};

    return $complex;
}

# ============================================================================
# Control Flow
# ============================================================================

sub control_flow_examples {
    my $value = 42;

    # if-elsif-else
    if ($value > 0) {
        say "Positive";
    } elsif ($value < 0) {
        say "Negative";
    } else {
        say "Zero";
    }

    # unless
    unless ($value == 0) {
        say "Not zero";
    }

    # Postfix conditions
    say "Positive" if $value > 0;
    say "Not zero" unless $value == 0;

    # Ternary
    my $result = $value > 0 ? 'positive' : 'non-positive';

    # given-when (switch)
    use feature 'switch';
    no warnings 'experimental::smartmatch';

    given ($value) {
        when (0)        { say "Zero" }
        when ([1..10])  { say "1-10" }
        when ($_ > 100) { say "Large" }
        default         { say "Other" }
    }

    # Loops
    for my $i (0..9) {
        next if $i == 5;
        last if $i == 8;
        say $i;
    }

    foreach my $item (@array) {
        say $item;
    }

    # C-style for
    for (my $i = 0; $i < 10; $i++) {
        say $i;
    }

    my $counter = 0;
    while ($counter < 5) {
        $counter++;
    }

    until ($counter >= 10) {
        $counter++;
    }

    do {
        $counter--;
    } while ($counter > 5);

    # Loop with labels
    OUTER: for my $i (0..9) {
        INNER: for my $j (0..9) {
            if ($i * $j > 50) {
                last OUTER;
            }
        }
    }

    return 1;
}

# ============================================================================
# File I/O
# ============================================================================

sub file_io_examples {
    my $filename = 'test.txt';

    # Write
    open(my $fh, '>', $filename) or die "Cannot open: $!";
    print $fh "Hello, File!\n";
    say $fh "Second line";
    close($fh);

    # Read entire file
    open(my $in, '<', $filename) or die "Cannot open: $!";
    my $content = do { local $/; <$in> };
    close($in);

    # Read line by line
    open($in, '<', $filename) or die "Cannot open: $!";
    while (my $line = <$in>) {
        chomp $line;
        say "Line: $line";
    }
    close($in);

    # Append
    open(my $append, '>>', $filename) or die "Cannot open: $!";
    say $append "Appended line";
    close($append);

    # Three-arg open with encoding
    open(my $utf8, '<:encoding(UTF-8)', $filename) or die "Cannot open: $!";
    close($utf8);

    # Autodie
    {
        use autodie;
        open(my $auto, '<', $filename);
        close($auto);
    }

    # Delete
    unlink $filename if -e $filename;

    return $content;
}

# ============================================================================
# Error Handling
# ============================================================================

sub error_handling_examples {
    # eval block
    my $result = eval {
        die "Something went wrong";
        return 42;
    };
    if ($@) {
        say "Error: $@";
    }

    # Try::Tiny
    try {
        die "Another error";
    }
    catch {
        say "Caught: $_";
    }
    finally {
        say "Cleanup";
    };

    # Warn
    warn "This is a warning";

    # Carp
    # croak "Fatal error with stack trace";
    # confess "Fatal error with full stack trace";

    return 1;
}

# ============================================================================
# Object-Oriented Examples
# ============================================================================

sub oo_examples {
    # Create objects
    my $point = Point->new(x => 1, y => 2, z => 3);
    say $point;
    say "Distance: " . $point->distance(Point->origin());

    my $point2 = Point->new(x => 4, y => 5, z => 6);
    my $sum = $point + $point2;
    say "Sum: $sum";

    my $person = Person->new(name => 'Alice', age => 30, email => 'alice@example.com');
    say $person->to_string();

    # Shapes
    my $circle = Circle->new(radius => 5, color => 'blue');
    $circle->draw();
    say "Area: " . $circle->area();

    my $rect = Rectangle->new(width => 4, height => 6);
    say $rect->describe();

    # Type checking
    if (blessed($circle) && $circle->isa('Shape')) {
        say "Circle is a Shape";
    }

    return 1;
}

# ============================================================================
# Main
# ============================================================================

# Variable declarations
my $scalar = 42;
my @array = (1, 2, 3, 4, 5);
my %hash = (one => 1, two => 2);

# Numeric literals
my $decimal = 1_000_000;
my $hex = 0xDEADBEEF;
my $octal = 0755;
my $binary = 0b10101010;
my $float = 3.14;
my $scientific = 1.23e-4;

# Strings
my $single = 'Hello';
my $double = "Hello, $scalar";
my $heredoc = <<'END';
This is a heredoc
with multiple lines
END

my $interpolated_heredoc = <<"END";
Value: $scalar
END

my $qw = qw(one two three);
my $qq = qq{Double quote: $scalar};

# String operations
my $concat = $single . ' ' . 'World';
my $repeat = 'x' x 10;
my $length = length($single);
my $upper = uc($single);
my $lower = lc($single);
my $substr = substr($single, 0, 3);

# Array operations
my $first = $array[0];
my $last = $array[-1];
my @slice = @array[1, 3];
my $joined = join(', ', @array);
my @sorted = sort @array;
my @reversed = reverse @array;
my @mapped = map { $_ * 2 } @array;
my @filtered = grep { $_ > 2 } @array;

# Hash operations
my $value = $hash{one};
my $default = $hash{missing} // 'default';
while (my ($k, $v) = each %hash) {
    say "$k => $v";
}

# Control flow
control_flow_examples();

# Regular expressions
regex_examples();

# Data structures
my $complex = data_structure_examples();
say Dumper($complex);

# File I/O
my $content = file_io_examples();

# Error handling
error_handling_examples();

# OO examples
oo_examples();

# Closures
my $double = make_multiplier(2);
my $triple = make_multiplier(3);
say "Double 5: " . $double->(5);
say "Triple 5: " . $triple->(5);

# Higher-order functions
my $result = apply_twice(sub { $_[0] + 1 }, 5);
say "Apply twice: $result";

# Timing
with_timing(sub {
    my $sum = 0;
    $sum += $_ for 1..1_000_000;
    return $sum;
});

# Counter with state
say "Counter: " . counter();
say "Counter: " . counter();
say "Counter: " . counter();

# List utilities
say "Sum: " . sum(1, 2, 3, 4, 5);
say "Max: " . max(1, 2, 3, 4, 5);
say "Min: " . min(1, 2, 3, 4, 5);

# JSON
my $data = { name => 'Test', values => [1, 2, 3] };
my $json_str = encode_json($data);
say "JSON: $json_str";
my $decoded = decode_json($json_str);

# Command line args
# GetOptions(
#     'help|h' => sub { pod2usage(1) },
#     'verbose|v' => \my $verbose,
# ) or pod2usage(2);

say GREETING;
say "Program completed successfully!";

__END__

=head1 LICENSE

This is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
