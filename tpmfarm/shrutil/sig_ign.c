#include <sys/signal.h>

void sig_ign_(int *sigsend)
{
	int status, sig;
	sig = *sigsend;
	signal(sig,SIG_IGN);
}
