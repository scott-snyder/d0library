#include <sys/signal.h>

void sig_set_(int *sigsend)
{
	void onintr(void);
	int status, sig;
	sig = *sigsend;
	signal(sig,onintr);
}
void onintr(void) 
{}
