use IO::Prompt;

# This example demostrates how prompt autosets $_ when appropriate

# Case 1...
my $first = prompt "1> ";
print defined() ? "(\$ was '$_')\n" : "(\$_ was undef)\n";
print "Got: $first\n";

# Case 2...
prompt "2> ";
print defined() ? "(\$ was '$_')\n" : "(\$_ was undef)\n";
print "Got: [$_]\n";

# Case 3...
while (prompt "3> ") {
    print defined() ? "(\$_ was '$_')\n" : "(\$_ was undef)\n";
    print "Got: [$_]\n";
    last if $_ ne "\n";
}

# Case 4...
while (my $next = prompt "4> ") {
    print defined() ? "(\$_ was '$_')\n" : "(\$_ was undef)\n";
    print "Got: $next\n";
    last if $next ne "\n";
}
