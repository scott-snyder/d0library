/*	I N T E G E R   F U N C T I O N   R D _ S O C K E T

	Read a message in this processor sent by another through a
	TCP connection. The socket through which the data is received
	must already be created, bound to a local address, and listening to
	the transmitting computer, and must have accepted a connection.

	integer*4  socket_descr
	integer*4  in_buffer (leng_buffer/4)
	integer*4  byte_count, rd_socket

	byte_count = rd_socket (socket_descr, in_buffer, %val(buffer_bytes))

	sock_descr is the descriptor (like a unit number) of a created,
	 bound, connected socket.
	in_buffer is an array into which the received data is placed.
	buffer_bytes is the number of bytes in the input buffer.
	byte_count is the number of bytes actually received

	This routine does not convert the data transmitted from network
	standard byte ordering. You must do that if it's necessary.

	If the byte count is negative, the reception failed; the value
	returned is -errno (the standard error number). If the byte
	count is positive, the reception probably succeeded.
	
*/

#include <unistd.h>

int
 rd_socket_ (socket_descr, in_buffer, buffer_bytes)
 int *socket_descr ;
 int in_buffer[ ] ;
 int buffer_bytes ;

{
 int byte_count ;
 extern int errno ;
 
 byte_count = read (*socket_descr, in_buffer, buffer_bytes) ;

 if (byte_count < 0) 
    byte_count = -errno ;

 return (byte_count) ;
}
