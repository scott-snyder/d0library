#include "smg.h"

#ifdef __NCURSES_H
#define _tmarg _regtop
#define _bmarg _regbottom
#endif

int wscrl(WINDOW *window, int nscroll)

/*
C----------------------------------------------------------------------
C-
C-   Name: wscrl
C-
C-   Purpose: This is a replacement for the undocumented curses routine of 
C-            the same name which doesn't work for pads.
C-
C-   Returned value: OK or ERR
C-
C-   Arguments: window  - pointer to window or pad.
C-              nscroll - Number of lines to scroll.
C-
C-   Created   1-OCT-1991   Herbert Greenlee
C-   Modified 28-SEP-1994   sss - work with ncurses
C-
C----------------------------------------------------------------------
*/
{
  int curx, cury;         /* Cursor position */

  if(nscroll == 0)
    return OK;

/* Remember cursor position. */

  getyx(window, cury, curx);

/* Simulate scrolling by deleting and inserting lines at the top and bottom of
   the scroll region. */

/* Forward scrolling done here. */

  while(nscroll > 0) {
    --nscroll;
    wmove(window, window->_tmarg, window->_begx);
    if(wdeleteln(window) == ERR)
      return ERR;
    wmove(window, window->_bmarg, window->_begx);
    if(winsertln(window) == ERR)
      return ERR;
  }

/* Backward scrolling done here. */

  while(nscroll < 0) {
    ++nscroll;
    wmove(window, window->_bmarg, window->_begx);
    if(wdeleteln(window) == ERR)
      return ERR;
    wmove(window, window->_tmarg, window->_begx);
    if(winsertln(window) == ERR)
      return ERR;
  }

/* Restore cursor position. */

  wmove(window, cury, curx);
  return OK;
}
