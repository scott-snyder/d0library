/*	L O G I C A L   F U N C T I O N   S H U T D O W N  _ S O C K E T

	Completely shutdown a socket connection

	integer*4  socket_descr
	integer*4  status, shutdown_socket
	status = shutdown_socket (socket_descr)

	If the shutdown succeeds, the status returned is 0. If there is
	an error, the standard error number -errno is returned.

*/


#include <sys/types.h>
#include <sys/socket.h>

int
 shutdown_socket_ (socket_descr)
 int *socket_descr ;

{
 int status ;
 int how = 2 ;
 extern int errno ;

 status = shutdown (*socket_descr, how) ;

 if (status != 0)  status = -errno ;

 return (status) ;
}
