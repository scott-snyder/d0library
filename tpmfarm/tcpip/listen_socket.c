/*	I N T E G E R   F U N C T I O N   L I S T E N _ S O C K E T

	Assert this programs's willingness to listen to input on the
	specified socket. A backlog of up to max_backlog requests may be
	accumulated before the socket will refuse further connection
	requests.

	integer*4  status, listen_socket
	integer*4  socket_descr, max_backlog
	status = listen_socket (socket_descr, max_backlog)

	The status returned is 0 for success, standard error code -errno
	in case of failure.

*/

 listen_socket_ (socket_descr, max_backlog)
 int *socket_descr, *max_backlog ;

{
 int status ;
 extern int errno ;

 status = listen (*socket_descr, *max_backlog) ;

 if (status != 0)  status = -errno ;

 return (status) ;
}
