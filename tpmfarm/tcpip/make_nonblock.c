/*	I N T E G E R   F U N C T I O N   M A K E  _ N O N B L O C K

	Create a TCP socket which can be used for inter-processor
	communication using the TCP/IP Internet network. Only the
	Internet address family is supported. Only the stream data
	protocol is supported. Only the standard protocol for
	Internet stream-oriented data transmission is supported.

	If the socket creation succeeds, the function returns a
	non-negative socket descriptor (similar in purpose to a 
	fortran i/o unit number) which can be used to identify the
	socket in future operations on the socket.

	If the socket creation fails, the function returns a negative
	value which is -errno, the standard error number.

	integer*4  socket_descr, make_socket
	socket_descr = make_socket ()
*/
	

#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <errno.h>

int make_nonblock_ ()

{
 int af=AF_INET, type=SOCK_STREAM, protocol=0, descr ;
 extern int errno ;

 descr = socket (af, type, protocol) ;
 if (descr < 0)  descr = -errno ;
 if(fcntl(descr,F_SETFL,FNDELAY) != 0) descr = -errno; /* non-blocking */
 return (descr) ;
}
