/*
 *-
 *-   Purpose and Methods: Install a signal handler which traps SIGSEGV.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  12-Apr-1994   John D. Hobbs
 *-
*/
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
int stdout_path;

#include <signal.h>
struct sigaction my_segv_action,def_segv_action;

#include <unix.h>
#if D0FLAVOR==IBMAIX
# define PIDCHAR 'a'
#else
# define PIDCHAR 'p'
#endif

volatile int dumpme;
int run,event;

void
segv_install_(int *force_dump)
{
  void segv_trap(int sig);
  stdout_path = fileno(stdout);
  dumpme = *force_dump;
  run=0;
  event=0;
  my_segv_action.sa_handler = segv_trap;
  sigemptyset(&my_segv_action.sa_mask);
  sigaction(SIGSEGV,&my_segv_action,&def_segv_action);
}

/*
 *-
 *-   Purpose and Methods: Print a traceback dump if a segmentation violation
 *-     occurs.  Most of the routines called from segv_trap are not guaranteed
 *-     to be reentrant...  (See the POSIX standard for more details)
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  12-Apr-1994   John D. Hobbs
 *-
*/
void segv_trap(int signum)
{
  char command[255];

  /* Unnecessary(?) safety measure. */
  if( signum != SIGSEGV ) return;     

  /* Do our own traceback stuff. */
  fprintf(stderr,"Segmentation violation occurred in run %d, event %d\n",run,event);
  fflush(stdout);
  fflush(stderr);
  sprintf(command,"echo 'set $page=0 ; func segv_trap; where; q' | 'dbx' -c /dev/null -%c %d | sed '1,/^>/ d' ",PIDCHAR,getpid());
  system(command);
  fflush(stdout);
  fflush(stderr);

  /* Exit with core file an option. */
  if( dumpme ) abort();
  else         exit(SIGSEGV);

}

void segv_set_run_event_(int *idrun, int *idevent) 
{
  run = *idrun;
  event = *idevent;
}
