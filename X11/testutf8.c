/* (c) Ricardas Cepas <rch@pub.osf.lt>. Copying policy: BSD or GNU GPL V2. */
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>

int 
main (void)
{
  unsigned int y = 0, x = 0, Isatty = 0;
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

      write(1, "\r\303\266", 3);
      write(1, "\33[6n", 4);
      
      scanf ("\033[%u;%u", &y, &x);/* get cursor position */

      tcsetattr (STDOUT_FILENO, TCSANOW, &termios_orig);

      write(1, "\r  \r", 4);
      

      /*Get a single byte in UTF-8 and 3 bytes othewise */
      switch (x)
	{
	case 2: /* UTF-8 */
          return 2;
          break;
	case 3: /* single-byte mode */
        case 4:
          return 1;
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

