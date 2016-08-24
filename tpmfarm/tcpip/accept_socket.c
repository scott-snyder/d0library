/*	I N T E G E R   F U N C T I O N   A C C E P T _ S O C K E T

	Accept a connection from a specified socket, and return a new
	socket descriptor from which requests may be read, data sent, etc.
	In this simplified routine, the identity of the calling party is
	not returned or checked.

	integer*4  new_socket_descr, accept_socket
	integer*4  old_socket_descr
	new_socket_descr = accept_socket (old_socket_descr)

	If new_socket_descr is greater than zero, it is a valid descriptor
	from which requests and data may be read. If it is less than zero,
	an error has occurred and -errno (standard error number) is 
	returned instead.

*/

#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>

int
 accept_socket_ (old_descr)
 int *old_descr ;

{
 struct sockaddr new_sock ;
 int leng_new_sock ;
 int new_descr ;
 extern int errno ;

 leng_new_sock = sizeof (new_sock) ;

 bzero (&new_sock, sizeof (new_sock)) ;

 new_descr = accept (*old_descr, &new_sock, &leng_new_sock) ;

 if (new_descr < 0)  new_descr = -errno ;

 return (new_descr) ;
}
