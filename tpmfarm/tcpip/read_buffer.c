#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>

int read_buffer_(int sock_des, int *buffer, int bytes)
{
	int nleft;
	int nread =0;
	int counter;
	char *ptr;


	nleft = bytes;
	ptr   = (char *)buffer;

	while(nleft > 0) {
		if((nread = read(sock_des, ptr, nleft)) <= 0) {
			if(nread == 0)
				return(-1);
			else
				if((bytes-nleft) > 0 && errno == EWOULDBLOCK) 
					continue;	
				else
					break;	
		}
		nleft -= nread;
		ptr   += nread;
	}
	return(bytes-nleft);
}
