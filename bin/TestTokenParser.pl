#!/usr/bin/perl
use File::Basename;
push @INC, dirname($0);
require "TokenParser.pm";

$tokenParser = TokenParser->newFile($ARGV[0]);
$tokenParser->{debug} = 1;
while ($type = $tokenParser->nextToken())
{
}
