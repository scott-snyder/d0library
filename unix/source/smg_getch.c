#include "smg.h"

int smg_getch(WINDOW *window, int isecho)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_getch
C-
C-   Purpose: Read a character from from curses input.
C-
C-   Returned value: Termination character code or ERR (defined in curses.h).
C-
C-   Usage:
C-
C-     char = smg_getch(window);
C-
C-   Arguments: 
C-
C-     window    - Pointer to curses window.
C-
C-   Created   30-SEP-1991   Herbert Greenlee
C-
C-   This routine is used internally by the smg emulattion package.  Its
C-   use is similar to the standard curses routine wgetch.  Smg_getch 
C-   reads a character using wgetch.  As far as possible, curses espace
C-   sequence codes (as defined in curses.h) are translated to smg terminator
C-   codes (as defined in $smgdef).  Untranslatable codes are returned
C-   as SMG$K_TRM_UNKNOWN = 0x1ff.
C-
C----------------------------------------------------------------------
*/

{
  int i;
  int ch;                                   /* Character read */

/* Escape sequence translation table */

  static int table[] = {
    KEY_LEFT, SMG$K_TRM_LEFT,
    KEY_RIGHT, SMG$K_TRM_RIGHT,
    KEY_UP, SMG$K_TRM_UP,
    KEY_DOWN, SMG$K_TRM_DOWN,
    KEY_ENTER, SMG$K_TRM_CR,
    KEY_NPAGE, SMG$K_TRM_NEXT_SCREEN,
    KEY_PPAGE, SMG$K_TRM_PREV_SCREEN,
    KEY_F(1), SMG$K_TRM_PF1,         /* PF key mapping for VT100 */
    KEY_F(2), SMG$K_TRM_PF2,
    KEY_F(3), SMG$K_TRM_PF3,
    KEY_F(4), SMG$K_TRM_PF4,
    KEY_F(5), SMG$K_TRM_F5,
    KEY_F(6), SMG$K_TRM_F6,
    KEY_F(7), SMG$K_TRM_F7,
    KEY_F(8), SMG$K_TRM_F8,
    KEY_F(9), SMG$K_TRM_PF1,         /* Normal PF key mapping for SGI */
    KEY_F(10), SMG$K_TRM_PF2,
    KEY_F(11), SMG$K_TRM_PF3,
    KEY_F(12), SMG$K_TRM_PF4,
    KEY_F(13), SMG$K_TRM_F13,
    KEY_F(14), SMG$K_TRM_F14,
    KEY_F(15), SMG$K_TRM_F15,
    KEY_F(16), SMG$K_TRM_F16,
    KEY_F(17), SMG$K_TRM_F17,
    KEY_F(18), SMG$K_TRM_F18,
    KEY_F(19), SMG$K_TRM_F19,
    KEY_F(20), SMG$K_TRM_F20,
    0
  };

/* Read and echo character.  Only printing characters are echoed. */

  ch = wgetch(window);
  if(isecho && ch >= ' ' && ch < 0x7f)pechochar(window, ch);

/* Translate ascii null and line feed to carriage return. */

  if(ch == '\0' || ch == '\n')
    ch = '\r';

/* Return ascii characters, otherwise more translation. */

  if(ch < 0x100 )
      return ch;

/* Translate escape sequences */

  for(i=0;; i+=2) {
    if(table[i] == 0)
      break;
    if(ch == table[i])
      return table[i+1];
  }

/* Unknown escape sequence */

  return SMG$K_TRM_UNKNOWN;
}
