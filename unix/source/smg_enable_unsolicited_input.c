#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <sys/socket.h>
#include <unistd.h>
#include "smg.h"

static void (*smg_ast_routine)(DISPLAY**, void*);
static void *smg_ast_argument;
static int signum;

static void smg_handler(int sig)

/* This routine is the actual interrupt handler. */

{
  chtype ch;
  DISPLAY *display;
  int input_ready;

/* Return immediately if interrupt processing is disabled. */

  if(!smg_static_data.async)
    return;
    
/* Find the top pasted window. */

  display = smg_display_root;
  while(display->next != NULL && display->next->pasted)
    display = display->next;

/* Test for input. */

  input_ready = 0;
  noecho();
  nodelay(display->window, TRUE);
  ch = wgetch(display->window);
  nodelay(display->window, FALSE);
  if(ch != ERR) {
    input_ready = 1;
    ungetch(ch);
  }

/* Call interrupt routine if input is ready. */

  if(input_ready)
    (*smg_ast_routine)(&smg_display_root, smg_ast_argument);
  return;
}

long smg$enable_unsolicited_input_( DISPLAY **pbid,
  void (*ast_routine)(DISPLAY**, void*), void *ast_argument)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$enable_unsolicited_input
C-
C-   Purpose: Enable asynchronous I/O.
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$enable_unsolicited_input(pbid, ast_routine, ast_argument)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only)       - Pasteboard identifier.
C-     ast_routine (external, read only) - External subroutine.
C-     ast_argument (any, read only)     - Passed to ast_routine.
C-
C-   Created   4-OCT-1991   Herbert Greenlee
C-
C-   This routine causes ast_routine to be called when input is available.
C-   Ast_routine must be declared EXTERNAL in the calling program.
C-
C----------------------------------------------------------------------
*/

{
  int optval, optlen, socket;
  struct sigaction act;

  smg_static_data.function_name = "smg$enable_unsolicited_input";

/* Verify pbid. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);

/* Save pointers to ast_routine and ast_argument. */

  smg_ast_routine = ast_routine;
  smg_ast_argument = ast_argument;

/* Enable signal generation on curses input stream using signal SIGUSR1. */
/*
  optlen = sizeof(optval);
  optval = 0;
  getsockopt(fileno(smg_static_data.cin), SOL_SOCKET, SO_TYPE, 
	     &optval, &optlen);
  socket = (optval == SOCK_STREAM);
  signum = SIGUSR1;
  if(!socket)
    if(ioctl(fileno(smg_static_data.cin), I_SETSIG, S_INPUT) < 0)
      return smg_error("ioctl failed", 0);
*/
/* Enable signal processing on input stream. */

  signum = SIGUSR1;
  act.sa_handler = smg_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  if(sigaction(signum, &act, NULL) < 0)
    return smg_error("sigaction failed", 0);

/* Set asynchronous flag. */

  smg_static_data.async = 1;
  return 1;
}
