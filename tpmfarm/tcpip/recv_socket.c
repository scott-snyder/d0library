/*	I N T E G E R   F U N C T I O N   R E C V _ S O C K E T

	Receive a message in this processor from another through a
	TCP connection. The socket through which the data is transmitted
	must already be created, bound to a local address, listening to
	the transmitting computer and must have accepted a message.

	integer*4  socket_descr
	integer*4  in_buffer (leng_buffer)
	integer*4  long_count, recv_socket

	long_count = recv_socket (socket_descr, in_buffer, leng_buffer)

	sock_descr is the descriptor (like a unit number) of a created,
	 bound, connected socket.
	in_buffer is an array into which the received data is placed.
	leng_buffer is the number of longwords in the input buffer.
	long_count is the number of longwords actually received

	This routine does not convert the data transmitted from network
	standard byte ordering. You must do that if it's necessary.

	If the longword count is negative, the reception failed; the value
	returned is -errno (the standard error number). If the longword
	count is positive, the reception probably succeeded.
	
*/

 recv_socket_ (socket_descr, in_buffer, buffer_longs)
 int *socket_descr ;
 int in_buffer[ ] ;
 int *buffer_longs ;

{
 int byte_count, long_count, buffer_bytes ;
 extern int errno ;
 
 buffer_bytes = *buffer_longs * 4 ;

 byte_count = recv (*socket_descr, in_buffer, buffer_bytes) ;

 if (byte_count < 0) 
    long_count = -errno ;
 else
    long_count = byte_count / 4 ;

 return (long_count) ;
}
