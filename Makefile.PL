#!/usr/bin/env perl

use inc::Module::Install;

name		('IO-Prompt');
author		('Damian Conway <dconway@cpan.org>');
abstract	('Interactively prompt for user input');
license		('perl');
version_from	('lib/IO/Prompt.pm');

requires        (
    perl            => 5.008,
    'Term::ReadKey' => 0,
);

WriteAll( sign => 1 );
