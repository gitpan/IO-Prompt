#! /usr/bin/perl -w

use IO::Prompt;

if (prompt -wipefirst, "first> ", -line) {
	print;
}


# Should not wipe screen, since previous call already did...

while (prompt -wipefirst, " next> ", -line) {
	print;
}
