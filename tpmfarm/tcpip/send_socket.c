/*	I N T E G E R   F U N C T I O N   S E N D _ S O C K E T

	Send a message from this processor to another through a
	TCP connection. The socket through which the data is transmitted
	must already be created, bound to a local address, and connected
	to the receiving machine.

	integer*4  socket_descr
	integer*4  out_buffer (leng_buffer)
	integer*4  status, send_socket

	status = send_socket (socket_descr, out_buffer, leng_buffer)

	sock_descr is the descriptor (like a unit number) of a created,
	 bound, connected socket.
	out_buffer is an array of longwords to be transmitted.
	leng_buffer is the number of longwords to be transmitted.

	This routine does not convert the data transmitted to network
	standard byte ordering. If that's important to you, do it first.

	If the status is negative, the transmission failed; the value
	returned is -errno (the standard error number). If the status is
	zero, there is a reasonable chance the transmission succeeded, but
	you don't know for sure until the server responds.

*/

 send_socket_ (socket_descr, out_buffer, buffer_longs)
 int *socket_descr ;
 int out_buffer[ ] ;
 int *buffer_longs ;

{
 int status, buffer_bytes ;
 extern int errno ;
 
 buffer_bytes = *buffer_longs * 4 ;

 status = send (*socket_descr, out_buffer, buffer_bytes) ;

 if (status < 0) status = -errno ;

 return (status) ;
}
