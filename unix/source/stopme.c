/*
 *-
 *-   Purpose and Methods: Install a signal handler which calls EVTIN_JOBSTOP
 *-     to shutdown a job gracefully.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created   6-Apr-1994   John D. Hobbs
 *-
*/
#include <unix.h>
#if D0FLAVOR==VAXVMS
#  define STOPME_INSTALL stopme_install
#  define EVTIN_JOBSTOP  evtin_jobstop
#else
#  define STOPME_INSTALL stopme_install_
#  define EVTIN_JOBSTOP  evtin_jobstop_
#endif

#include <signal.h>
struct sigaction my_action,def_action;

void
STOPME_INSTALL()
{
  void set_stop_flag(int sig);
  my_action.sa_handler = set_stop_flag;
  sigemptyset(&my_action.sa_mask);
  sigaction(SIGUSR2,&my_action,&def_action);
}


/*
 *-
 *-   Purpose and Methods: Call the EVENT_UTIL routine EVTIN_JOBSTOP to force
 *-     graceful shutdown when the next event is requested.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  12-Apr-1994   John D. Hobbs
 *-
*/
void set_stop_flag(int signum)
{
  void EVTIN_JOBSTOP();
  EVTIN_JOBSTOP();
  return;
}
