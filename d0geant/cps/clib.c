#include <signal.h>
#include <errno.h>
#include <time.h>

/* Fortran wrappers for C library functions */

void cperror_(char *string)
{
	perror(string);
}
cctime_(long int n, char *pc)
{
	strcpy(pc, ctime((const time_t*)n));
}
time_(long int n)
{
	time(0);
}
cgethostname_(char *host, long int n)
{
	gethostname(host,n);
}
unsigned int csleep_(unsigned int n)
{
	sleep(n);
}
