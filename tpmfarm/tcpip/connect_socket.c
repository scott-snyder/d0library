/*	I N T E G E R   F U N C T I O N   C O N N E C T _ S O C K E T

	Connect a socket already created to a specified port number
	of a specified processor. The port number is specified by a
	16-bit integer. The processor is specified by a 32-bit 
	internet address.

	If the socket has not been bound to a local address, this is
	done automatically before the connection is attempted. If the
	connection fails, the address thus bound is preserved.

	integer*4  connect_socket, status
	integer*4  socket_descr
	integer*2  port_number
	integer*4  internet_address
	status = connect_socket (socket_descr, port_number, internet_address)

	If the connection is made successfully, a status of 0 is returned.
	Errors return the system error number -errno.
*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>

connect_socket_ (socket_descr, port_number, internet_address)
 int *socket_descr ;
 ushort *port_number ;
 int *internet_address ;

{
 struct sockaddr_in conn_addr ;
 extern int  errno ;
 int status ;

 bzero (&conn_addr, sizeof (conn_addr)) ;

 conn_addr.sin_family      = AF_INET ;
 conn_addr.sin_port        = htons (*port_number) ;
 conn_addr.sin_addr.s_addr = htonl (*internet_address) ;

 status = connect (*socket_descr, (struct sockaddr*)&conn_addr, sizeof (conn_addr)) ;

 if (status != 0)  status = -errno ;

 return (status) ;
}
