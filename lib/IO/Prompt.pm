package IO::Prompt;

use 5.008;
use strict;
no warnings 'utf8';

our $VERSION   = '0.02';
our @EXPORT    = qw( prompt );
our @EXPORT_OK = qw( hand_print get_input );

use Carp;
use IO::Handle;
use Term::ReadKey;
use POSIX qw( isprint );

my $clearfirst;
my %input;

sub clear {
    return unless $_[0];
    open my $OUT, ">/dev/tty" or croak "Cannot write to terminal: $!";
    print {$OUT} "\n" x 60;
    $clearfirst = 0;
}

our %flags_arg = (
    p  => 'prompt',
    s  => 'speed',
    e  => 'echo',
    r  => 'require',
    d  => 'default',
    u  => 'until',
    w  => 'while',
    nl => 'newline',
);

our %flags_noarg = (
    y   => 'yes',
    n   => 'no',
    i   => 'integer',
    num => 'number',
    1   => 'onechar',
    w   => 'wipe',
    a   => 'argv',
    l   => 'line',
    t   => 'tty'
);

my $RECORD;    # store filehandle for __PROMPT__ file supporting -record flag

$flags_arg{$_}   = $_ for values %flags_arg;
$flags_noarg{$_} = $_ for values %flags_noarg;

my $flag_with_arg = join '|', reverse sort keys %flags_arg;
my $flag_no_arg   = join '|', reverse sort keys %flags_noarg;

my %yespat = (
    'y' => qr/^\s*[yY]/,
    'Y' => qr/^\s*Y/,
);

my %nopat = (
    'n' => qr/^\s*[nN]/,
    'N' => qr/^\s*N/,
);

my %num_pat = (
    integer => qr{[+-]? \d+ (?:[Ee][+-]?\d+ )?}x,
    number  => qr{[+-]? (?:\d+[.]?\d* | [.]\d+) (?:[Ee][+-]?\d+)? }x,
);

sub get_prompt (\%@) {
    my ($flags, @data) = @_;
    my ($OUT);
    for (my $i = 0 ; $i < @data ; $i++) {
        local *_ = \($data[$i]);
        if (ref eq 'HASH') {
            splice @data, $i + 1, 0, %$_;
        }
        elsif (ref eq 'GLOB' or UNIVERSAL::isa($_, 'IO::Handle')) {
            croak "Can't write prompt to read-only $_" unless -w;
            $OUT = $_;
        }
        elsif (/^-/) {
            if (s/^-(f|wipefirst)/-/) {
                $clearfirst = 1 unless defined $clearfirst;
            }
            elsif (s/^-(yes|y)/-/i) {
                $flags->{-yesno}{yes} = $yespat{ substr $1, 0, 1 };
                $flags->{-yesno}{yesprompt} = substr $1, 0, 1;
            }
            elsif (s/^-(?:nl|newline)/-/i) {
                $flags->{-nlstr} = $data[ $i + 1 ];
                undef $data[ $i++ ];
            }
            elsif (s/^-number|-num/-/i) {
                $flags->{-number} = 'number';
            }
            elsif (s/^-integer|-i/-/i) {
                $flags->{-number} = 'integer';
            }
            elsif (s/^-(no|n)/-/i) {
                $flags->{-yesno}{no} = $nopat{ substr $1, 0, 1 };
                $flags->{-yesno}{noprompt} = substr $1, 0, 1;
            }
            elsif (s/^-($flag_with_arg)/-/) {
                $flags->{ -$flags_arg{$1} } = $data[ $i + 1 ];
                undef $data[ $i++ ];
            }
            elsif (s/^-($flag_no_arg)/-/) {
                $flags->{ -$flags_noarg{$1} } = 1;
            }
            else { croak "Unknown flag ($_) in prompt" }

            redo if /^-./;
        }
        else { next }
        undef $data[$i];
    }
    $_ =
        !defined() ? undef
      : ref eq 'Regexp' ? $_
      : qr/^\Q$_\E$/
      for @{$flags}{qw(-while -until)};

    for (grep { defined } $flags->{ -require }) {
        croak "Argument to -require must be hash reference"
          unless ref eq 'HASH';
        my %reqs = %$_;
        $_ = sub {
            my ($input) = @_;
            for (keys %reqs) {
                return $_ unless smartmatch($input, $reqs{$_});
            }
            return;
        };
    }
    my @prompt = grep { defined } @data;
    if (@prompt && exists $flags->{-default}) {
        my $prompt = join "", @prompt;
        $prompt =~ s/(:?\s*)$/ [$flags->{-default}]$1/ if $prompt !~ /\[.*\]/;
        @prompt = $prompt;
    }
    return $OUT, @prompt;
}

my $prompt_req = "(The value entered is not acceptable) ";

sub prompt {
    my $caller = caller;
    my %flags;
    my ($OUT, @prompt) = get_prompt(%flags, @_);
    open $OUT, ">/dev/tty" or croak "Cannot write to terminal: $!" if !$OUT;
    $OUT->autoflush(1);
    @prompt = $flags{ -prompt } if !@prompt and $flags{ -prompt };
    my $IN;
    if ($flags{-tty} || $flags{-argv}) {
        open $IN, "</dev/tty" or croak "Cannot read from terminal: $!";
    }
    else {
        no strict 'refs';
        my $ARGV = $caller . "::ARGV";
        unless (*$ARGV->opened) {
            $$ARGV = shift(@$ARGV) || '-';
            open $ARGV or croak "Can't open $$ARGV: $!";
        }
        $IN = \*$ARGV;
        @prompt = () unless -t $IN;
    }
    $flags{-speed} = 0.075 unless defined $flags{-speed};
    $clearfirst = 1 if !defined($clearfirst) && $flags{-clearfirst};
    clear($flags{ -clear } || $clearfirst);
    my $input;
    if (-t $IN and exists $input{$caller}) {
        $input = fake_from_DATA($caller, $IN, $OUT, \%flags, @prompt);
    }
    elsif ($flags{-argv}) {
        return if @ARGV;
        @prompt = "Args for $0: " if -t $IN and !@prompt;
        print {$OUT} @prompt;
        @ARGV = map glob, split /\s+/, get_input($IN, $OUT, \%flags, @prompt);
        return @ARGV;
    }
    elsif ($flags{-yesno}) {
        return yesno($IN, $OUT, \%flags, @prompt);
    }
    elsif ($flags{-number}) {
        return number($IN, $OUT, \%flags, @prompt);
    }
    else {
        print {$OUT} @prompt;
        $input = get_input($IN, $OUT, \%flags, @prompt);
    }
    return tidy($input, %flags);
}

sub tidy {
    my ($input, %flags) = @_;
    my $defined = defined $input;
    chomp $input if $defined && !$flags{-line};
    my $success = $defined
      && (!$flags{ -while } || $input =~ $flags{ -while })
      && (!$flags{ -until } || $input !~ $flags{ -until });
    print {$RECORD} $input, "\n" if $success && $RECORD;
    return bless {
        value   => $input,
        success => $success,
        set_val => 1,
        context => (caller(1))[2],
      },
      'IO::Prompt::ReturnVal';
}

sub success {
    my ($val, $no_set) = @_;
    print {$RECORD} $val, "\n" if $val && $RECORD;
    return bless {
        value   => $val,
        success => 1,
        set_val => !$no_set,
        context => (caller(1))[2],
      },
      'IO::Prompt::ReturnVal';
}

sub failure {
    return bless {
        value   => $_[0],
        success => 0,
        set_val => 0,
        context => (caller(1))[2],
      },
      'IO::Prompt::ReturnVal';
}

sub import {
    my $class = shift;

    {
        no strict 'refs';
        *{ caller() . "::$_" } = \&{$_} for @EXPORT;

        foreach my $sym (@_) {
            grep { $_ eq $sym } @EXPORT_OK or next;
            *{ caller() . "::$sym" } = \&{$sym};
        }
    }

    @_ = grep /^-/, @_;
    $input{ caller() } = undef;
    $clearfirst = 1 and return if "@_" eq "-clearfirst";
    for my $i (0 .. $#_) {
        last if $RECORD;
        if ($_[$i] eq '-record') {
            splice @_, $i, 1;
            open $RECORD, '>', '__PROMPT__'
              or croak "Can't open __PROMPT__ recording file: $!";
            print {$RECORD} "__DATA__\n__PROMPT__\n";
        }
    }
    prompt @_ if @_;
}

CHECK {
    for my $pkg (keys %input) {
        next if defined $input{$pkg};

        no strict 'refs';
        if (my $datahandle = *{"${pkg}::DATA"}{IO}) {
            local $/;
            my $data = <$datahandle>;
            if ($data =~ s/(\A|\n) __PROMPT__ \s*? \n (.*)/$1/xs) {
                $input{$pkg} = "$2";
            }
            else {
                delete $input{$pkg};
            }
            open "${pkg}::DATA", "<", \$data or die "Internal error: $!";
        }
        else {
            delete $input{$pkg};
        }
    }
}

my $baseline = ord 'A';

sub hand_print {
    my $OUT   = \*STDOUT;
    my $echo  = 0;
    my $speed = 0.05;
    local $| = 1;
    for (@_) {
        if (ref eq 'HASH') {
            $speed = $_->{-speed} if exists $_->{-speed};
            $OUT   = $_->{-to}    if exists $_->{-to};
            $echo  = $_->{-echo}  if exists $_->{-echo};
        }
        elsif (!$speed) {
            print {$OUT} $_;
        }
        else {
            print {$OUT} $_ and select undef, undef, undef, rand $speed
              for map {
                    defined $echo ? $echo
                  : isprint($_)   ? $_
                  : $_ eq "\n" ? $_
                  : ord($_) == 0        ? ''
                  : ord($_) < $baseline ? '^' . chr($baseline + ord($_) - 1)
                  : '?'
              } split "";
        }
    }
    return scalar @_;
}

sub fake_from_DATA {
    my ($caller, $IN, $OUT, $flags, @prompt) = @_;
    local $SIG{INT} = sub { ReadMode 'restore', $IN; exit };
    ReadMode 'noecho', $IN;
    ReadMode 'raw',    $IN;
    print {$OUT} @prompt;
    my $input = getc $IN;
    unless (defined $input) { print {$OUT} "\n"; return; }
    if ($input eq "\e") {
        ReadMode 'restore', $IN;
        return get_input($IN, $OUT, $flags, @prompt);
    }
    $input{$caller} =~ m/\G (?!\cD|\cZ) (.*) (\n?)/xgc;
    my ($line, $nlstr) = ($1, $2);
    unless (defined $line) {
        while ($input ne "\n") { $input = getc $IN }
        print {$OUT} "\n";
        return;
    }
    delete $input{$caller} if pos $input{$caller} == length $input{$caller};
    if ($input eq "\n") {
        hand_print { -to => $OUT, %$flags }, $line;
        unless (defined <$IN>) { print {$OUT} "\n"; return; }
    }
    else {
        my $i = 0;
        while (1) {
            my $done = $i >= length $line;
            print {$OUT} substr($line, $i++, 1) unless $done;
            last if getc $IN eq "\n" && $done;
        }
    }
    ReadMode 'restore', $IN;
    print {$OUT} "\n";
    return $line . $nlstr;
}

sub get_input {
    my ($IN,      $OUT,   $flags, @prompt)  = @_;
    my ($onechar, $nlstr, $echo,  $require) =
      @{$flags}{ -onechar, -nlstr, -echo, -'require' };
    $nlstr = "\n" unless defined $nlstr;
    if (!-t $IN) {
        return scalar <$IN> unless $onechar;
        return getc $IN;
    }
    $OUT->autoflush(1);
    local $SIG{INT} = sub { ReadMode 'restore', $IN; exit };
    my ($input, $newlines);
    my %cntl = GetControlChars $IN;
    my $cntl = join '|', values %cntl;
    ReadMode 'raw', $IN;
  INPUT: while (1) {
        my $next = getc $IN;
        if ($next eq $cntl{INTERRUPT}) {
            ReadMode 'restore', $IN;
            exit;
        }
        elsif ($next eq $cntl{ERASE} and length $input) {
            substr($input, -1) = "";
            print {$OUT} "\b \b";
            next;
        }
        elsif ($next eq $cntl{EOF}) {
            ReadMode 'restore', $IN;
            close $IN;
            return $input;
        }
        elsif ($next !~ /$cntl/ && defined $next) {
            $input .= $next;
            if ($next eq "\n") {
                if ($input eq "\n" && exists $flags->{-default}) {
                    print {$OUT}(
                        defined $echo
                        ? $echo x length($flags->{-default})
                        : $flags->{-default}
                    );
                    print {$OUT} $nlstr;
                    ReadMode 'restore', $IN;
                    return $onechar ? substr($_, 0, 1) : $_
                      for $flags->{-default};
                }
                $newlines .= $nlstr;
            }
            else {
                print {$OUT}(defined $echo ? $echo : $next);
            }
        }
        else {
            $input .= $next;
        }
        if ($onechar or !defined $next or $input =~ m{\Q$/\E$}) {
            chomp $input unless $flags->{-line};
            if ($require and my $mesg = $require->($input)) {
                print {$OUT} "\r", " " x 79, "\r", sprintf($mesg, @prompt);
                undef $input;
                undef $newlines;
            }
            else {
                ReadMode 'restore', $IN;
                print {$OUT} $newlines;
                return $onechar ? substr($input, 0, 1) : $input;
            }
        }
    }
}

sub yesno {
    my ($IN,  $OUT, $flags,     @prompt)   = @_;
    my ($yes, $no,  $yesprompt, $noprompt) =
      @{ $flags->{ -yesno } }{qw(yes no yesprompt noprompt)};
    $yes = qr/^([^Nn])/ unless defined $yes;
    $no  = qr/^([^Yy])/ unless defined $no;
    my $prompt2 =
        $yesprompt && $noprompt ? "'$yesprompt' or '$noprompt'"
      : $yesprompt ? "'$yesprompt' for yes"
      : "'$noprompt' for no";
    print {$OUT} @prompt if -t $IN;
    while (1) {
        my $response =
          get_input($IN, $OUT, { %$flags, -nlstr => "" }, @prompt);
        chomp $response unless $flags->{-line};
        print {$OUT} "\n" and return success($response, 'no_set')
          if defined $response
          and $response =~ /$yes/;
        print {$OUT} "\n" and return failure($response)
          if !defined $response
          or $response =~ /$no/;
        print {$OUT} "\r", " " x 79, "\r", @prompt,
          "(Please answer $prompt2) "
          if -t $IN;
    }
}

sub number {
    my ($IN, $OUT, $flags, @prompt) = @_;
    my $numtype    = $flags->{ -number };
    my $prompt_num = "(Please enter a valid $numtype) ";
    my $match      = $num_pat{$numtype};
    my $require    = $flags->{ -require };
    print {$OUT} @prompt if -t $IN;
    while (1) {
        my $response =
          get_input($IN, $OUT, { %$flags, -nlstr => "", -require => undef },
            @prompt);
        chomp $response if defined $response && !$flags->{-line};
        if (-t $IN and defined $response) {
            if ($response !~ /\A \s* $match \s* \Z/x) {
                print {$OUT} "\r", " " x 79, "\r", @prompt, $prompt_num;
                next;
            }
            elsif ($require and my $mesg = $require->($response)) {
                print {$OUT} "\r", " " x 79, "\r", sprintf($mesg, @prompt);
                next;
            }
        }
        print {$OUT} "\n" and return tidy($response);
    }
}

sub smartmatch {
    my ($str, $matcher) = @_;
    my $type = ref $matcher;
    my $res = $type eq 'CODE'
      ? do { local $_ = $str; $matcher->() }
      : $type eq 'Regexp' ? ($str =~ $matcher)
      : $type eq 'ARRAY' ? scalar grep({ smartmatch($str, $_) } @$matcher)
      : $type eq 'HASH' ? $matcher->{$str}
      : $str eq $matcher;
    return $res;
}

package IO::Prompt::ReturnVal;

use overload q{bool} => sub {
    $_ = $_[0]{value} if $_[0]{set_val};
    $_[0]{handled} = 1;
    $_[0]{success};
  },
  q{""} => sub { $_[0]{handled} = 1; "$_[0]{value}"; },
  q{0+} => sub { $_[0]{handled} = 1; 0 + $_[0]{value}; },
  fallback => 1,
  ;

sub DESTROY {
    $_ = $_[0]{value} unless $_[0]{handled};
}

1;
__END__

=head1 NAME

IO::Prompt - Interactively prompt for user input

=head1 VERSION

This document describes version 0.02 of IO::Prompt, released
September 28, 2004.

=head1 SYNOPSIS

    use IO::Prompt;
    while( prompt "next: " ) {
        print "You said '$_'\n";
    }

=head1 DESCRIPTION

By default, this module exports a single function C<prompt>.  It prompts the
user to enter some input, and returns an object that represents the user input.

You may specify various flags to the function to affect its behaviour; most
notably, it defaults to automatically C<chomp> the input, unless the C<-line>
flag is specified.

Two other functions are exported at request: C<hand_print>, which simulates
hand-typing to the console; and C<get_input>, which is the lower-level function
that actually prompts the user for a suitable input.

Please consult the F<examples> directory from this module's CPAN distribution
to better understand how to make use of this module.

=head2 Arguments to C<prompt>

 Flag   Long form      Arg          Effect
 ----   ---------      ---          ------
                       <str>        Use <str> as prompt

                       <filehandle> Prompt to specified filehandle

                       <hashref>    Flatten hash entries into argument list
                                    (useful for aggregating the options below)

 -p     -prompt        <str>        Specify prompt explicitly

 -s     -speed         <num>        Simulated typing speed (seconds/char)

 -e     -echo          <str>        What to echo for each char typed

 -nl    -newline       <str>        When a newline is typed, echo <str> instead

 -d     -default       <str>        What to return if only <return> pressed


 -r     -require       <hashref>    Each value of each entry must 'smartmatch'
                                    the input else corresponding key is printed
                                    as error message:
                                     - Subs must return true when passed input
                                     - Regexes must pattern match input
                                     - Strings must eq match input
                                     - Arrays are flattened & recursively matched
                                     - Hashes must return true for input as key

 -u     -until         <str|rgx>    Fail if input matches <str|regex>
 -w     -while         <str|rgx>    Fail unless input matches <str|regex>

 -1     -onechar                    Return immediately after first char typed

 -w     -wipe                       Clear screen before prompt
 -f     -wipefirst                  Clear screen before first prompt only

 -a     -argv                       Load @ARGV from input if @ARGV empty

 -l     -line                       Don't autochomp

 -t     -tty                        Prompt to terminal no matter what

 -y     -yes                        Return true if [yY] entered, false otherwise
 -yn    -yesno                      Return true if [yY], false if [nN]
 -Y     -Yes                        Return true if 'Y' entered, false otherwise
 -YN    -YesNo                      Return true if 'Y', false if 'N'

 -num   -number                     Accept only valid numbers as input
 -i     -integer                    Accept only valid integers as input

Flags can be "cuddled". For example:

     prompt("next: ", -tyn1s=>0.2)   # -tty, -yes, -no, -onechar, -speed=>0.2

=head1 CAVEATS

Currently, there are no meaningful tests and documentation for this module.
Contributions will be very much appreciated.

=head1 AUTHOR

Damian Conway (damian@conway.org)

=head1 MAINTAINERS

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>,
Brian Ingerson E<lt>INGY@cpan.orgE<gt>.

=head1 COPYRIGHT

   Copyright (c) 2004, Damian Conway. All Rights Reserved.
 This module is free software. It may be used, redistributed
     and/or modified under the same terms as Perl itself.
