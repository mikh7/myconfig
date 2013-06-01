#!/usr/bin/perl -s
#
# Convert the syncolor.vim file and for any line containinc gui= guifg etc
# commands insert the closest match cterm* command for the 256 color
# xterm
#
if (! defined $ARGV[0]) {
  print "Usage: $0 [-exact|-exact=<start index>] <vim color script>

        Parses the vim file and replaces each line containing both
        cterm* and gui* lines. Deletes the exitsting cterm* options
        from such lines and replaces them with the cterm* option
        for 256color xterm that match the gui* option
        
        If -exact is not specified the nearest color is used.
        No modification of default 256 xterm colors are needed, but
        some colors may be a bit off.
        
        If -exact is specified then if no exact xterm256
        default color is found to match the gui* color, then 
        it generates an .XResources file entry that has to be put 
        into your .Xresources file. The xterm256 colors are modified
        starting from <start index> which defualts to 232 (the start 
        of the grayscale ramp in defualt xterm256 palette)
        
        ";
  exit 1;
}

open FIN, $ARGV[0] or die "Unable to open $ARGV[0]: $!";
my @lines = <FIN>;
chomp @lines;
close FIN;

# load the rgb.txt
open FIN, "/usr/share/X11/rgb.txt" or die "$!";
while (<FIN>) {
  chomp;
  if (/^\s*!/) {
    next;
  }
  my @a = split;
  while ($#a != -1 and $a[0] eq '') {
    shift @a;
  }
  die "Invalid line $_" if ($#a < 3);
  my ($r, $g, $b);
  $r = shift @a;
  $g = shift @a;
  $b = shift @a;
  my $name = join ' ', @a;
  $rgb_colors{uc $name} = [$r, $g, $b];
}
close FIN;

sub get_named_color {
  my $c = shift;
  die "Unknown color name '$c'" if !defined $rgb_colors{uc $c};
  return $rgb_colors{uc $c};
}

# initialize the defualt 256 xterm colors

$xterm_colors[0] = get_named_color('black');
$xterm_colors[1] = get_named_color('red3');
$xterm_colors[2] = get_named_color('green3');
$xterm_colors[3] = get_named_color('yellow3');
$xterm_colors[4] = get_named_color('blue3');
$xterm_colors[5] = get_named_color('magenta3');
$xterm_colors[6] = get_named_color('cyan3');
$xterm_colors[7] = get_named_color('gray90');
$xterm_colors[8] = get_named_color('gray30');
$xterm_colors[9] = get_named_color('red');
$xterm_colors[10] = get_named_color('green');
$xterm_colors[11] = get_named_color('yellow');
$xterm_colors[12] = [0x5c, 0x5c, 0xff];
$xterm_colors[13] = get_named_color('magenta');
$xterm_colors[14] = get_named_color('cyan');
$xterm_colors[15] = get_named_color('white');

for ($red = 0; $red < 6; $red++) {
  for ($green = 0; $green < 6; $green++) {
    for ($blue = 0; $blue < 6; $blue++) {
      $code = 16 + ($red * 36) + ($green * 6) + $blue;
      $r = ($red ? ($red * 40 + 55) : 0);
      $g = ($green ? ($green * 40 + 55) : 0);
      $b = ($blue ? ($blue * 40 + 55) : 0);
      $xterm_colors[$code] = [$r, $g, $b];
      $xterm_used[$code] = 0;
    }
  }
}

$code=232;
for ($gray = 0; $gray < 24; $gray++) {
  $level = ($gray * 10) + 8;
  $code = 232 + $gray;
  $xterm_colors[$code] = [$level, $level, $level];
  $xterm_used[$code] = 0;
}

#for ($i = 0; $i < 256; $i++) {
#  print "Xterm color #$i = " . join(',', @{$xterm_colors[$i]}) . "\n";
#}

sub get_nearest_xterm_color {
  my $color = shift;
  my $rgb;
  if (defined $rgb_colors{uc $color}) {
    $rgb = $rgb_colors{uc $color}
  } elsif ($color =~ /^#(..)(..)(..)$/) {
    $rgb = [hex $1, hex $2, hex $3];
  } else {
    die "Unknown color $color";
  }
  my @rgb_target = @{$rgb};
  my $best_d = 100000;
  my $best = -1.0;
  for ($i = 0; $i < 256; $i++) {
    my @x = @{$xterm_colors[$i]};
    my $d1 = 0.0 + abs($rgb_target[0] - $x[0]);
    my $d2 = 0.0 + abs($rgb_target[1] - $x[1]);
    my $d3 = 0.0 + abs($rgb_target[2] - $x[2]);
    my $d = sqrt($d1*$d1 + $d2*$d2 + $d3*$d3);
    if ($d < $best_d) {
      $best = $i;
      $best_d = $d
    }
  }
  print "Best match for color $color is xterm color $best \x1b[48;5;${best}m     \x1b[0m\n";
  return $best;
}

sub rgb {
  my $color = shift;
  if (defined $rgb_colors{uc $color}) {
    $rgb = $rgb_colors{uc $color}
  } elsif ($color =~ /^#(..)(..)(..)$/) {
    $rgb = [hex $1, hex $2, hex $3];
  } else {
    die "Unknown color $color";
  }
}

sub get_exact_xterm_color {
  my $color = shift;
  my $rgb = rgb($color);
  my @rgb_target = @{$rgb};
  for ($i = 0; $i < 256; $i++) {
    my @x = @{$xterm_colors[$i]};
    if ($rgb_target[0] == $x[0]
      && $rgb_target[1] == $x[1]
      && $rgb_target[2] == $x[2])
    {
      return $i;
    }
  }
  return undef;
}

my $start_index = $exact >15 ? $exact : 232;

sub get_xterm_replacement_color {
  if ($exact) {
    my $color = get_exact_xterm_color(@_);
    if (not defined $color) {
      while ($xterm_used[$start_index] && $start_index < 256) {
        $start_index++;
      }
      if ($start_index == 256) {
        print "Out of modifieable xterm color indexes, specify a smaller
        index in -exact parameter\n";
        exit 2;
      }
      $xterm_used[$start_index] = 1;
      $xterm_colors[$start_index] = rgb(@_);
      my @tmp = @{rgb(@_)};
      $xterm_modified[$start_index] = sprintf("#%2.2x%2.2x%2.2x", $tmp[0], $tmp[1], $tmp[2]);
      return $start_index++;
    } else {
      $xterm_used[$color] = 1;
      return $color;
    }
  } else {
    return get_nearest_xterm_color(@_);
  }
}

foreach (@lines) {
  #print "Orig: $_\n";
  s/cterm[a-z]*=\S*\s*//g;
  s/term[a-z]*=\S*\s*//g;
  if (/gui=(\S+)/) {
    $_ .= " cterm=$1";
  }
  if (/(guibg)=(\S+)/) {
    my $repl = (uc $2 eq 'NONE' or uc $2 eq 'BG') ? $2 : get_xterm_replacement_color($2);
    $_ .= " ctermbg=" . $repl;
  }
  if (/(guifg)=(\S+)/) {
    my $repl = (uc $2 eq 'NONE' or uc $2 eq 'BG') ? $2 : get_xterm_replacement_color($2);
    $repl = 'NONE' if uc $2 eq 'BG';
    $_ .= " ctermfg=" . $repl;
  }
  #print "New : $_\n";
}

open OUT , ">$ARGV[0]" or die "Unable to create $ARGV[0]: $!";
print OUT join("\n", @lines);
close OUT;

if ($exact) {
  my $code;
  my $first = 1;
  for ($code=0; $code < 255; $code++) {
    if (defined $xterm_modified[$code]) {
      if ($first) {
        print "
# Put the below entris into your .Xresources file
# and run xrdb ~/.Xresources\n";
        $first = 0;
      }
      my @tmp = @{rgb($xterm_modified[$code])};
      printf("\x1b]4;%d;rgb:%2.2x/%2.2x/%2.2x\x1b\\",
        ${code}, $tmp[0], $tmp[1], $tmp[2]);
      print "xterm*color${code}: $xterm_modified[$code]\n";
    }
  }
}
