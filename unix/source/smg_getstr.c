#include "smg.h"

int smg_getstr(WINDOW *window, char *string, int max_chars, int isecho,
	       long term_mask)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_getstr
C-
C-   Purpose: Read a string from from curses input.
C-
C-   Returned value: Termination character code or ERR (defined in curses.h).
C-
C-   Usage:
C-
C-     term_char = smg_getstr(window, string);
C-
C-   Arguments: 
C-
C-     window    - Pointer to curses window (for echoing).
C-     string    - Pointer to input buffer.
C-     max_chars - Maximum number of characters to read (not incl. 
C-                 terminator).
C-     isecho    - Echo flag.
C-     term_mask - Terminator set bit mask.
C-
C-   Created   26-AUG-1991   Herbert Greenlee
C-
C-   This routine is used internally by the smg emulattion package.  Its
C-   use is similar to the standard curses routine wgetstr.  Smg_getstr 
C-   reads characters using wgetch until a string terminator is encountered.
C-   A string terminator is defined as any of the following:
C-
C-   1. Any ascii control character whose corresponding bit is set in 
C-      term_mask.
C-
C-   2. Any escape sequence (e.g. arrow, function and keypad keys).
C-
C-   On normal return, a null-terminated string of characters preceding
C-   the terminator are returned in string.  The terminator is returned 
C-   as the value of the function.  As far as possible, curses espace
C-   sequence codes (as defined in curses.h) are translated to smg terminator
C-   codes (as defined in $smgdef).  Untranslatable terminators are returned
C-   as SMG$K_TRM_UNKNOWN = 0x1ff.  The erase and line kill characters are
C-   interpreted locally.
C-
C----------------------------------------------------------------------
*/

{
  int ch;                                   /* Character read */
  int n=0;                                  /* Number of chararacter in 
                                               string */
  static int first = 1, erasec, killc;      /* Erase and kill characters */

/* One time initialization */

  if(first) {
    first = 0;
    erasec = erasechar();
    killc = killchar();
}

/* Read characters loop */

  while(n < max_chars) {
    ch = smg_getch(window, isecho);

/* Check for erase and kill */

    if(ch == erasec) {
      if(n > 0) {
        --n;
	pechochar(window, '\b');
	pechochar(window, ' ');
	pechochar(window, '\b');
      }
      continue;
    }
    if(ch == killc) {
      while(n > 0) {
        --n;
	pechochar(window, '\b');
	pechochar(window, ' ');
	pechochar(window, '\b');
      }
      continue;
    }

/* Check for string terminators */

    if( ch < ' ' && (term_mask & 1 << ch) || ch >= 0x100)
      break;

/* None of the above conditions hold.  Add character to string. */

    string[n++] = ch;
  }

/* Now we are out of the read loop.  Terminate string */

  string[n] = '\0';

/* Decide what termination condition to return */

/* Buffer full? */

  if(n >= max_chars)
    return SMG$K_TRM_BUFFER_FULL;
  return ch;
}

