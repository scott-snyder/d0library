#include <sys/types.h>
#include <signal.h>

int check_pid_(int pid)
{
	pid_t pidp;
	int stat;

	pidp = pid;
	stat = kill(pidp,0);
	
	return(stat);

}
