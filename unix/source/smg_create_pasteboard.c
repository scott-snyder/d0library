#define _ALL_SOURCE 1   /* Needed on IBM to process sys/socket.h */
#include "smg.h"
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

DISPLAY *smg_display_root = NULL;        /* Root of global display chain. */
SMG_DATA smg_static_data;

static void smg_exit_handler(int sig)

/* This routine catches signals that normally cause an abort or exit. */

{
  struct sigaction act;

/* Drop out of visual mode. */

  smg_reset_term_();

/* Kill child process (if any). */

  if(smg_static_data.child > 0)
    kill(smg_static_data.child, SIGKILL);

/* Set signal action to default and resignal the current process. */

  act.sa_handler = SIG_DFL;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  sigaction(sig, &act, NULL);
  kill(getpid(), sig);
  return;
}

long smg$create_pasteboard_( DISPLAY **pbid, char *outdev, long *nrows, 
  long *ncols, long *flags, long outdev_len)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$create_pasteboard
C-
C-   Purpose: Initialize curses output
C-
C-   Returned value  : 0 (false)     - Operation failed.
C-                     1 (true)      - Operation succeeded.
C-                     SMG$_PASALREXI - Terminal already open.
C-
C-   Fortran usage:
C-
C-      ok = smg$create_pasteboard(pbid, outdev, nrows, ncols, flags)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, write only)  - Pasteboard identifier.
C-     outdev (character, read only) - Output device.  Unimplemented.
C-     nrows (integer*4, write only) - Number of rows in screen.
C-     ncols (integer*4, write only) - Number of columns in screen.
C-     flags (integer*4, read only) -  Preserve screen flag (0 = clear, 
C-                                     1=preserve).
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C-   This function initializes curses by calling initscr.  It should be 
C-   the first smg$ function called.  This function creates a virtual terminal
C-   connected to standard input and output.  Only one virtual terminal 
C-   may be open at a time.  If a virtual terminal is already open, this
C-   function returns SMT$_PASALREXI rather than SS$_NORMAL.  In this case
C-   the the already open pasteboard identifier is returned in pbid.
C-
C----------------------------------------------------------------------
*/

{
  long rval;                     /* Return value */
  FILE *cout, *cin;              /* Curses output and input streams */
  char *tty_device, *tty_type;   /* Terminal device and type */
  int sock, sock1;               /* Socket descriptors */
  int n;
  char parent[8];                /* Parent process id */
  struct sockaddr sockname;      /* Socket name structure */
  char ttyname[20];              /* Child terminal name */

/* Signal stuff. */

  int i, sig;                    /* Signal number. */             
  struct sigaction act, oact;    /* POSIX signal structure. */

/* Here is a list of normally fatal POSIX signals. */

  const int numfatal=12;
  static int sigfatal[12] = {SIGABRT, SIGALRM, SIGFPE,  SIGHUP,  
			     SIGILL,  SIGINT,  SIGPIPE, SIGQUIT,
			     SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2};

  smg_static_data.function_name = "smg$create_pasteboard";

/* Do we need to initialize curses? */

  if(smg_display_root == NULL) {

  /* Yes.  Initialize curses and create the root display structure. */

  /* Establish a exit/abort handlers for POSIX signals that normally cause 
     abnormal termination if they still have the default action. */

    act.sa_handler = smg_exit_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    for(i=0; i<numfatal; ++i) {
      sig = sigfatal[i];
      if(sigaction(sig, NULL, &oact) < 0)
	abort();
      if(oact.sa_handler == SIG_DFL)
	if(sigaction(sig, &act, NULL) < 0)
	  abort();
    }

  /* Ignore SIGUSR1 until it is enabled by smg$enable_unsolicited_input. */

    act.sa_handler = SIG_IGN;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if(sigaction(SIGUSR1, &act, NULL) < 0)
      abort();

  /* Get terminal device and terminal type information from the environment. */

    tty_type = getenv("SMGTERM");
    if(tty_type == NULL)
      tty_type = getenv("TERM");
    if(tty_type == NULL)
      tty_type = "vt100";
    tty_device = getenv("SMGTTY");
    if(tty_device == NULL)
      tty_device = "";

  /* Make a named UNIX domain socket for communicating with child process. */

    sock = socket(PF_UNIX, SOCK_STREAM, 0);
    if(sock < 0)
      abort();
    sockname.sa_family = PF_UNIX;
    sprintf(sockname.sa_data, "/tmp/smg%d", (int)getpid());
    if(bind(sock, &sockname, 
	    strlen(sockname.sa_data) + sizeof(sockname.sa_family)) < 0)
      abort();
    if(listen(sock, 1) < 0)
      abort();

  /* Fork a child process to run stdintofifo. */

    smg_static_data.child = fork();
    if(smg_static_data.child < 0)
      abort();
    if(smg_static_data.child == 0) {

  /* In child. */

      sprintf(parent, "%d", (int)getppid());
      if(!strcmp(tty_device, "wsh")) {
	execlp("/bin/wsh", "/bin/wsh", 
	       "-Z1", 
	       "-t", "SMG", 
	       "-c", "stdintosock", sockname.sa_data, parent,
	       (char *)0);
      }
      else if(!strcmp(tty_device, "xterm")) {
	execlp("xterm", "xterm", 
	       "+ls", 
	       "-j",
	       "-s",
	       "-sb",
	       "-sk",
	       "-title", "SMG", 
	       "-e", "stdintosock", sockname.sa_data, parent,
	       (char *)0);
      }
      else if(!strcmp(tty_device, "aixterm")) {
	execlp("aixterm", "aixterm", 
	       "+ls", 
	       "-j",
	       "-s",
	       "-sb",
	       "-sk",
	       "-title", "SMG", 
	       "-fullcursor",
	       "-v",
	       "-e", "stdintosock", sockname.sa_data, parent,
	       (char *)0);
      }
      else
	execlp("stdintosock", "stdintosock", sockname.sa_data, parent, (char *)0);
      fprintf(stderr, "exec failed\n");
    }

  /* Back in parent.  Open FIFO for input. */

    sock1 = accept(sock, NULL, 0);
    if(sock1 < 0)
      abort();
    unlink(sockname.sa_data);

  /* Read characters until null to get output terminal name. */

    n = -1;
    do
      if(read(sock1, &ttyname[++n], 1) < 0 || n > 20)
	abort();
    while(ttyname[n] != '\0');

  /* Open input and output streams for curses virtual terminal. */

    if(n == 0)
      cout = stdout;
    else
      cout = fopen(ttyname, "w");
    if(cout == NULL)
      abort();
    cin = fdopen(sock1, "r");
    if(cin == NULL)
      abort();

  /* Allocate space for root display structure. */

    smg_display_root = malloc(sizeof *smg_display_root);
    if(smg_display_root == NULL)
      return smg_error("memory allocation failed", 0);

  /* Initialize curses. */

    if(newterm(tty_type, cout, cin) == NULL)
      return smg_error("error returned by newterm", 0);

  /* Fill in global data. */

    smg_static_data.batch = 0;
    smg_static_data.cin = cin;
    smg_static_data.cout = cout;

  /* Fill in data for root display. */

    smg_display_root->window = stdscr;
    if(smg_display_root->window == NULL)
      return smg_error("error returned by initscr", 0);
    smg_display_root->brdr = NULL;
    smg_display_root->title = NULL;
    smg_display_root->nrows = LINES;
    smg_display_root->ncols = COLS;
    smg_display_root->vrows = smg_display_root->nrows;
    smg_display_root->vcols = smg_display_root->ncols;
    smg_display_root->row_vport = 0;
    smg_display_root->col_vport = 0;
    smg_display_root->dattr = 0;
    smg_display_root->vattr = 0;
    smg_display_root->pasted = 0;
    smg_display_root->prow = 0;
    smg_display_root->pcol = 0;
    smg_display_root->num = 0;
    smg_display_root->nscroll = 0;
    smg_display_root->next = NULL;
    smg_display_root->prev = NULL;
    if(wclear(smg_display_root->window) == ERR)
      return smg_error("error returned by wclear", 0);
  }

/* Set options for standard screen. */

  if(keypad(smg_display_root->window, TRUE) == ERR)
    return smg_error("error returned by keypad", 0);
  if(noecho() == ERR)
    return smg_error("error returned by noecho", 0);


/* See if we need to paste the standard display.  This will ordinarily clear
   the screen.  The standard screen will be in an unpasted state only if 
   this is the first call to smg$create_pasteboard_ or if 
   smg$delete_pasteboard_ has previously been called. */

  if(!smg_display_root->pasted) {
    smg_display_root->pasted = 1;
    if(smg_update(smg_display_root) == 0)
      return 0;
    rval = 1;
  }
  else

  /* Curses already initialized.  Use alternate return code. */

    rval = SMG$_PASALREXI;

/* Return values to calling program. */

  if(pbid != NULL)
    *pbid = smg_display_root;
  if(nrows != NULL)
    *nrows = LINES;
  if(ncols != NULL)
    *ncols = COLS;
  return rval;
}
