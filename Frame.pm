package Frame;

use strict;
use base qw (Exporter);
our @EXPORT = qw (frame);

sub frame
{
  my ($t, $width) = @_;
  my $len = length ($t);

  my $df = 3;
  $width ||= $len + 2 * $df;

  my $line1 = '*' . ('-' x ($width-2)) . '*';
  my $line2 = '|' . (' ' x ($width-2)) . '|';


  my $TEXT = '';

  $TEXT .= "$line1\n";
  for (1 .. ($df-1)/2)
    {
      $TEXT .= "$line2\n";
    }

  die ("Cannot frame text: `$t'\n")
    if ($width - 2 * $df <= 0);
  

  while (length ($t))
    {
      my $s = substr ($t, 0, $width - 2 * $df, '');

      my $i = 0;
      while (length ($s) < $width - 2 * $df)
        {
          if ($i % 2)
            {
              $s = " $s";
            }
          else
            {
              $s = "$s ";
            }
          $i++;
        }
      my $linet = '|' . (' ' x ($df-1)) . $s .  (' ' x ($df-1)) . '|';
      $TEXT .= "$linet\n";
    }

  for (1 .. ($df-1)/2)
    {
      $TEXT .= "$line2\n";
    }
  $TEXT .= "$line1\n";
}


1;

