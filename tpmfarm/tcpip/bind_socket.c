/*	I N T E G E R   F U N C T I O N   B I N D _ S O C K E T

	Bind a system-chosen address and a specified port number
	to a TCP socket.

	integer*4  status, bind_socket
	integer*4  socket_descriptor, port_number
	status = bind_socket (socket_descriptor, port_number)

	The socket descriptor was assigned the socket when it was opened.
	See procedure make_socket.

	The local address is assigned by the system (INADDR_ANY).

	The port number is chosen by the user. It must be between 1024
	and 5000 and unique within the farm.

	The status returned is zero for success, standard error code -errno
	in case of failure.

*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int  bind_socket_ (descr, port_number)
int *descr, *port_number ;

{
 int status ;
 extern int errno ;
 struct sockaddr_in sock ;

 bzero (&sock, sizeof(sock)) ;

 sock.sin_family      = AF_INET ;
 sock.sin_addr.s_addr = INADDR_ANY ;
 sock.sin_port        = htons (*port_number) ;

 status = bind (*descr, &sock, sizeof (sock)) ;

 if (status != 0)  status = -errno ;

 return (status) ;
}
