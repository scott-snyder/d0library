/*
C-----------------------------------------------------------------------
C-
C- Name      : d0echo
C-
C- Purpuse   : This program is similar to standard UNIX echo programs
C-             and built-in shell commands, but with better transportability
C-             In particular, the following aspects of echo behaviour are
C-             standardized:
C-
C-             1.  Multiple arguments are assembled into one line with
C-                 a single space between arguments and a terminal new
C-                 line character after the last argument.  An empty 
C-                 argument list produces an output of a single new line 
C-                 character.
C-
C-             2.  All standard c-escapes are translated into their single 
C-                 character equivalents: \n, \t, \v, \b, \r, \f, \a, \\, 
C-                 \nnn (octal) and \xnn (hex).  
C-
C-             3.  The escape \c truncates the output line, including the 
C-                 terminal new line.
C-
C-             4.  Occurances of the backslash character not covered by
C-                 the previous two rules are left unchanged.
C-
C- Usage:
C-
C-     d0echo arg1 arg2 ...
C-
C- Created 4-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/

#include <stdlib.h>
#include <string.h>

#define STRLEN 4096
#define OCTAL(a) ((a)>='0' && (a) < '8')
#define HEX(a) (isxdigit(a))

int main(int argc, char **argv, char **envp) {
  int i, n;
  char *back;             /* Pointer to next backslash. */
  char total, next;       /* For octal and hex escapes. */
  char line[STRLEN];      /* Input arg list. */

/* Scan argument list.  Construct output line. */

  line[0] = '\0';
  for(i=1; i<argc; ++i) {
    if(line[0] == '\0')
      strcpy(line, argv[i]);
    else {
      strcat(line, " ");
      strcat(line, argv[i]);
    }
  }
  strcat(line, "\n");

/* Translate c-escapes. */

  back = &line[0]-1;
  while((back = strchr(back+1, '\\')) != NULL) {
    *back = '\0';

/* \n (new line)? */

    if(*(back+1) == 'n' ) {
      strcat(line, "\n");
      strcat(line, back+2);
    }

/* \t (tab)? */

    else if(*(back+1) == 't' ) {
      strcat(line, "\t");
      strcat(line, back+2);
    }

/* \v (vertical tab)? */

    else if(*(back+1) == 'v' ) {
      strcat(line, "\v");
      strcat(line, back+2);
    }

/* \b (backspace)? */

    else if(*(back+1) == 'b' ) {
      strcat(line, "\b");
      strcat(line, back+2);
    }
 
/* \r (carriage return)? */

    else if(*(back+1) == 'r' ) {
      strcat(line, "\r");
      strcat(line, back+2);
    }

/* \f (form feed)? */

    else if(*(back+1) == 'f' ) {
      strcat(line, "\f");
      strcat(line, back+2);
    }

/* \a (audible alert)? */

    else if(*(back+1) == 'a' ) {
      strcat(line, "\a");
      strcat(line, back+2);
    }

/* \\ (backslash)? */

    else if(*(back+1) == '\\' ) {
      strcat(line, "\\");
      strcat(line, back+2);
    }

/* \c (cut)? */

    else if(*(back+1) == 'c' ) {
      break;
    }

/* Octal? */

    else if(OCTAL(*(back+1))) {
      total = 0;
      for(n=1; n<=3 && OCTAL(*(back+n)); ++n) {
	next = *(back+n) - '0';
	total = 8*total + next;
      }
      *back = total;
      *(back+1) = '\0';
      strcat(line, back+n);
    }

/* Hex? */

    else if(*(back+1) == 'x') {
      total = 0;
      for(n=2; n<=3 && HEX(*(back+n)); ++n) {
	if(isdigit(*(back+n)))
	  next = *(back+n) - '0';
	else
	  next = tolower(*(back+n)) + 10 - 'a';
	total = 16*total + next;
      }
      *back = total;
      *(back+1) = '\0';
      strcat(line, back+n);
    }

/* Unrecognized */

    else
      *back = '\\';
  }

/* Done.  Print line. */

  printf(line);
  exit(0);

}
