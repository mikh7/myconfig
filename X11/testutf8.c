/* (c) Ricardas Cepas <rch@pub.osf.lt>. Copying policy: BSD or GNU GPL V2. */
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>

int 
testUTF8 (void)
{
  unsigned int y, x, Isatty = 0;
  struct termios termios_orig, termios;

  if (isatty (STDIN_FILENO) && isatty (STDOUT_FILENO))
    Isatty = 1;
  if (Isatty)
    {
      tcgetattr (STDOUT_FILENO, &termios_orig);
      termios = termios_orig;
      tcflush (STDOUT_FILENO, TCIOFLUSH);
      cfmakeraw (&termios);
      tcsetattr (STDOUT_FILENO, TCSANOW, &termios);
      /* ^X^Z cancel any ESC sequence */
      /* `A' from font directly via UTF-8; ask cursor position */
      printf ("\030\032" "\r\xEF\x81\x81" "\033[6n\033D");
      scanf ("\033[%u;%u", &y, &x);/* get cursor position */
      tcsetattr (STDOUT_FILENO, TCSANOW, &termios_orig);
      printf("\033[1F" "\033[%uX", (x-1)); /* go back; erase 1 or 3 char */
      fflush (stdout);
      /*Get a single byte in UTF-8 and 3 bytes othewise */
      switch (x)
	{
	case 2: /* UTF-8 */
		x=1;
		break;
	case 4: /* single-byte mode */
		x=0;
		break;
	default: /* error */
		x=255;
	}
    }
  else
    {
      x = 127; /* not a tty */
    }
return (x);
}

