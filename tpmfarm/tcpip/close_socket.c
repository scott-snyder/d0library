/*	L O G I C A L   F U N C T I O N   C L O S E _ S O C K E T

	Close a socket.

	integer*4  socket_descr
	integer*4  status, close_socket
	status = close_socket (socket_descr)

	If the close succeeds, the status returned is 0. If there is
	an error, the standard error number -errno is returned.

*/


 close_socket_ (socket_descr)
 int *socket_descr ;

{
 int status ;
 extern int errno ;

 status = close (*socket_descr) ;

 if (status != 0)  status = -errno ;

 return (status) ;
}
