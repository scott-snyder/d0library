/*	I N T E G E R   F U N C T I O N   S E L E C T _ S O C K E T

	Perform an Ultrix select operation, checking a specified group of
	socket descriptors (other descriptors too) to see if they are
	ready to be read, ready to be written, or posting exceptions.

	This is a complicated system call -- see select(2) for details.

	integer*4  n_found, select_socket
	integer*4  n_descr, read_mask, write_mask, exception_mask
	integer*4  timeout
	n_found = select_socket (n_descr, read_mask, write_mask, 
     +	 exception_mask, timeout)

	If the call succeeds, n_found is the number of descriptors found
	 that are ready to have the designated operations performed on them.
	 If the call fails, n_found is -errno, the standard error number.

	n_descr is the number of mask bits to check, starting with bit 0.
	On the CREMF farms, the maximum number of file descriptors is 64;
	thus n_descr cannot be more than 64, and the three masks may be
	arrays of two integer*4 longwords.

	read_mask has a 1 in each bit position corresponding to a 
 	 descriptor to be checked for read-readiness.

	write_mask works the same, but for write-readiness.

	exception_mask works the same, but for exception detection.

	timeout is the time (in seconds) to wait before returning even
	 if no ready descriptors have been found. Set to 0 for instant
	 polling.


	The mask bits are assigned to descriptor numbers as follows:
	  descriptor 0    00000001 hex  first longword
          descriptor 1    00000002 hex  first longword
	  descriptor 2    00000004 hex  first longword
	  descriptor 3    00000008 hex  first longword
	  descriptor 4    00000010 hex  first longword
               :              :
	  descriptor 31   80000000 hex  first longword
	  descriptor 32   00000001 hex  second longword
               :              :
	  descriptor 63   80000000 hex  second longword

	More descriptors than that? You're out of luck unless you
	reconfigure Unix.


	When the routine returns, n_found tells you how many ready
	descriptors you found. If it's zero, wait awhile and try again
	or find something else to do.

	In any case, the three masks are changed (so don't send in
	constants or anything you want to keep!). Those bits are set
	whose devices are ready to be read/written/tested.

*/

#include <sys/time.h>


 select_socket_ (n_descr, read_mask, write_mask, exc_mask, timeo)
 int *n_descr ;
 int *read_mask, *write_mask, *exc_mask ;
 int *timeo ;

{
 int n_found ;
 extern int errno ;
 struct timeval timeout ;

 timeout.tv_sec  = *timeo ;
 timeout.tv_usec = 0 ;

 n_found = select (*n_descr, (fd_set*)read_mask, (fd_set*)write_mask, (fd_set*)exc_mask, &timeout) ;

 if (n_found < 0)  n_found = -errno ;

 return (n_found) ;
}
