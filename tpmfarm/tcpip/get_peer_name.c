/*	I N T E G E R   F U N C T I O N   G E T _ P E E R _ N A M E 

	Checks if the binded socket is still connected, and return 
        the status of the call. In the second argument it also
        returns the name of the node

	integer*4  status, get_peer_name, socket_descriptor	
        character*20 host_name
	status = get_peer_name (socket_descriptor, host_name)

	The status is 0 if the call succeeds, standard error number errno 
        if it fails.
        Kirill Denisenko, 06/16/93
*/

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int get_peer_name_ (int *socket_fd, char *host_name)
{
 int status ;
 extern int errno ;
 struct sockaddr_in *remote_peer;
 struct sockaddr_in peer_remote;
 struct hostent *host_info;
 int *address_length;
 int length = 100;
 int fd;
 char array[100];

 fd = *socket_fd;
 address_length = &length;
 remote_peer = &peer_remote;
 
 status = getpeername (fd, (struct sockaddr*)remote_peer, address_length) ;
 if (status != 0)  
    status = errno ;
 else {
    host_info = gethostbyaddr(&peer_remote.sin_addr, length, AF_INET);
    if(host_info != NULL)
    	strcpy(host_name, host_info->h_name);
    else
	status = h_errno;
 }
    

 return (status) ;
}
