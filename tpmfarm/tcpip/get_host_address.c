/*	F U N C T I O N   G E T _ H O S T _ A D D R E S S

	Get the IP address for the host "host_name" 

	integer*4  host_address, get_host_address	
	character*31 host_name
	host_address = get_host_address (host_name)

	The host name is 1-31 characters long. 
*/

#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

 int get_host_address_ (host_name)
 char *host_name;

{
 int status;
 unsigned long inet_as;
 struct hostent *host_info;
 struct in_addr myaddr;
 char  *host_address;


 host_info = gethostbyname ( host_name );

 bcopy ( host_info->h_addr_list[0], (char *) &myaddr,
           host_info->h_length);
 
 host_address = inet_ntoa ( myaddr );
 inet_as      = inet_addr ( host_address );


 return (inet_as) ;
}
