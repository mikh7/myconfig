#!/usr/bin/perl

# Author:	maximus.mikhanosha
# Purpose:	parse the revisions file
# Created:	09/21/2006 02:13:14



while (<>) {
  chomp;
  if (/back-?end/) {
    $backend = 1;
    next;
  }
  if (/front-?end/) {
    $backend = 0;
    $frontend = 1;
    next;
  }  
  if (/^New rev ([\d.]+),?\s+previous rev ([\d.]+)/
|| /^First revision, (1.0)/) {
    my $anew = $1;
    my $old = defined($2) ? $2 : 'None';
    #print STDERR "New rev $anew $old\n";
    $fname{$fname} = {
      fname => $fname,
      old => $old,
      anew => $anew,
    } if (not defined $fname{$fname});
    $fname{$fname}->{anew} = $anew;
    $fname{$fname}->{old} = $old 
      if (not defined $fname{$fname}->{old});
  }
  elsif (/^(\S+)/)
  { 
    #print STDERR "Fname=$1\n";
    $fname = $1;
  }
  else {  
    #print STDERR "Ignored line $_\n";
  }
}

foreach $fname (keys %fname)
{
  my $tmp = $fname{$fname};
  printf "$fname\t$$tmp{old}\t$$tmp{anew}\n";
}
