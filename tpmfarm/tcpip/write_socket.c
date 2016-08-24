/*	I N T E G E R   F U N C T I O N   W R I T E _ S O C K E T

	Write a data buffer to another processor through a socket. The
	socket must be created, bound to an address, and connected.

	integer*4  byte_count, write_socket, socket_descr
	integer*4  buffer (n_longs), n_longs
	byte_count = write_socket (socket_descr, buffer, n_longs)

	A longword count is specified; a byte count is returned if the
	write is successful. If the operations fails, -errno (standard
	error number) is returned instead.

*/

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

int
 write_socket_ (descr, buffer, n_longs)
 int *descr ;
 int buffer [] ;
 int *n_longs ;

{
 int bytes_sent, longs_sent ;
 extern int errno ;
 int n_bytes ;

 n_bytes = *n_longs * 4 ;

 bytes_sent = write (*descr, buffer, n_bytes) ;

 if (bytes_sent < 0)
    longs_sent = -errno ;
 else
    longs_sent = bytes_sent / 4 ;
 

 return (longs_sent) ;
}
