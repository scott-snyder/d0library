#include <sys/types.h>
#include <signal.h>

/* f-callable interface to a c-lib kill routine */
/* K. Denisenko, 02/09/94                       */

int kill_(int *pid, int *sig)
{
	pid_t pidc;
	pidc = *pid;
	return(kill(pidc, *sig));
}
