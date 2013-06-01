
package TokenParser;

sub new
{
  my $class = shift;
  die "Constructor called on initialized object\n"
    if ref $class;
  my $text = shift;
  my $self = {
    token   => '',
    line    => '',
    lineNum => 0,
    type    => '',
    text    => $text,
    returnEof => 0
  };
  bless $self, $class;
}

sub newFile
{
  my $class = shift;
  my $fname = shift;
  local *INFILE;
  open INFILE, "<$fname" or die "Unable to open $fname\n";
  my @text = <INFILE>;
  close INFILE;
  my $self = new($class, \@text);
  $self->{fname} = $fname;
  return $self;
}

  #my $line = shift @{$self->{text}};

sub _nextToken
{
  my $self = shift;
  while (1)
  {
    if ($self->{line} eq '')
    {
      $self->{col} = 0;
      unless ($self->{lineNum} < $#{$self->{text}})
      {
        if ($self->{returnEof} == 1)
        {
          $self->{token} = '';
          return ($self->{type} = 'EOF');
        }
        else
        {
          $self->{lineNum}++;
          die "Unexpected end of file at line $self->{lineNum}\n";
        }
      }
      else
      {
        $self->{line} = $self->{text}->[$self->{lineNum}++];
      }
      chomp $self->{line};
      next;			
    }
# remove leading spaces
    if ($self->{line} =~ /^\s+/)
    {
      $self->{col} += length($&);
      $self->{line} = $';
      next;
    }
    # one line comment 
    if ($self->{line} =~ /^\/\//)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($self->{line});
      $self->{token} = $self->{line};
      $self->{line} = '';
      return ($self->{type} = 'comment');
    }
    # preprocessor directive
    if ($self->{line} =~ /^#/)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($self->{line});
      $self->{token} = $self->{line};
      $self->{line} = '';
      return ($self->{type} = 'cpp');
    }
    # multi-line comment
    if ($self->{line} =~ /^\/\*/)
    {
      $self->{startLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{line} = $';
      $self->{token} = '/*';
      $self->{col} += 2;
      while ()
      {
        if ($self->{line} =~ /\*\//)
        {
          $self->{col} += length($`) + 2;
          $self->{endLine} = $self->{lineNum};
          $self->{endCol} = $self->{col};
          $self->{token} .= $` . "*/";
          $self->{line} = $';
          return ($self->{type} = 'comment');
        }
        $self->{token} .= "$self->{line}\n";
        $self->{col} = 0;
        $self->{line} = $self->{text}->[$self->{lineNum}++]
          or die "Runaway comment started at line $startLine\n";
        chomp $self->{line};				
      }
    }
    if ($self->{line} =~ /^(\.\.\.)/) {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($1);
      $self->{col} += length($1);
      $self->{token} = $1;
      $self->{line} = $';
      return ($self->{type} = 'ident');
    }
    # word
    if ($self->{line} =~ /^(~?[A-Za-z_](?:[A-Za-z_0-9.\*]|::~?)*(?:\[[0-9]*\])?)/)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($1);
      $self->{col} += length($1);
      $self->{token} = $1;
      $self->{line} = $';
      return ($self->{type} = 'ident');
    }
    # punctuation
    if ($self->{line} =~ /^(
            (?:==)| (?:!=)| (?:>=)| (?:<=)| (?:\&\&)| (?:\|\|)|
            (?:=\+)| (?:=-)| (?:=\*)| (?:=\/)| (?:<<)| (?:>>)|
            (?:=\~=)|
            (?:[=!<>+\-\/*^&|()[\]{};?:,%.\~]))/x)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($1);
      $self->{col} += length($1);
      $self->{token} = $1;
      $self->{line} = $';
      return ($self->{type} = 'punct');
    }
    # number
    if ($self->{line} =~ /^[0-9][0-9.]*/)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($&);
      $self->{col} += length($&);
      $self->{token} = $&;
      $self->{line} = $';
      return ($self->{type} = 'number');
    }
    # string
    if ($self->{line} =~ /^"(?:(?:\\")|(?:[^"]))*"/)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($&);
      $self->{col} += length($&);
      $self->{token} = $&;
      $self->{line} = $';
      return ($self->{type} = 'string');
    }
    if ($self->{line} =~ /^'(?:(?:[^'])|(?:\\'))*'/)
    {
      $self->{startLine} = $self->{endLine} = $self->{lineNum};
      $self->{startCol} = $self->{col};
      $self->{endCol} = $self->{col} + length($&);
      $self->{col} += length($&);
      $self->{token} = $&;
      $self->{line} = $';
      return ($self->{type} = 'char');
    }
    die "Unknown stuff at line $self->{lineNum}: $self->{line}\n";
  }
}

sub last_token
{
  my $self = shift;
  return {
    token => $self->{token},
    startLine => $self->{startLine},
    endLine => $self->{endLine},
    startCol => $self->{startCol},
    endCol => $self->{endCol}
  };
}

sub nextToken
{
  my $self = shift;
  $self->_nextToken();
  if ($self->{debug})
  {
    print "$self->{type}: $self->{token}\n";
  }
  return $self->{type};
}

sub merge_token {
  my $self = shift;
  my $tok1 = shift;
  my $tok2 = shift;
  $tok1->{endLine} = $tok2->{endLine};
  $tok1->{endCol} = $tok2->{endCol};
  $tok1->{token} .= $tok2->{token};
};

sub nextTokenSkipComments
{
  my $self = shift;
  $self->nextToken();
  while ($self->{type} eq 'comment')
  {
    $self->nextToken();
  }
  return $self->{type};
}

return 1;

# vim: sw=2 cindent softtabstop=2 nowrap expandtab
