#include <stdio.h>
#include <stdlib.h>
#define IBMAIX 2
#if D0FLAVOR == IBMAIX
        #define HDR <sys/select.h>
#else
        #define HDR <sys/types.h>
#endif
#include HDR
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <strings.h>
#include "pfarms.h"
/*******************************************************************************

                       Name:  inreader.c

Mark Galli, Kirill Denisenko, 8/17/93

          Calling sequence:  inreader.x  key_in, pid, node, port
                             Where key_in is the key to be associated
                             with the inreader's shared memory segment,
                             and pid is reco's pid;
                             node is the client node name ( e.g., fnsfe )
                             port is the port on which connection is to
                             be made

*******************************************************************************/


int main(  int argc, char *argv[] )
{
	static int event_rqst[10]  = { 3, 0, 0, 0, 0, 0, 0, 0, 0, 0 } ;
	static int record_rqst[10] = { 5, 0, 0, 0, 0, 0, 0, 0, 0, 0 } ;

	key_t key;
	int piden;
	int opperm_flags;
	int shmid, shmidaux, size, i;
	unsigned long addr, rtrn_adr;
	int *buffer, *buffer_aux;
	pid_t ppid, mypid;

        int af=AF_INET, type=SOCK_STREAM, protocol=0;
	struct sockaddr_in conn_addr ;
	int port;
	u_short port_number;
        int socket_descr;

	unsigned long inet_as;
	struct hostent *host_info;
	struct in_addr myaddr;
	char  *host_address;

	int t_count, gt_count, at_count;
	int numrec, numaux;
	int keyaux;
	int *bptr;
	unsigned bytr;

/*  Function Prototypes                                            */
	int read_buf(int , int *, int);
	void onintr_inr(int);
	/*void bzero(void *, int );*/
	/*void bcopy(const void *, void *, int);*/

/*   Check number of arguments  */
	if ( argc != 5 ) {
		printf("[INREADER]: Wrong calling sequence.  Exiting...\n"); 
		exit(-1);
     	}

/*   Convert key and pid to integers  */
	sscanf(argv[1],"%d",&key);
	sscanf(argv[2],"%d",&piden);
	ppid  = piden;

/*   Convert port number to integer   */
	sscanf(argv[4],"%u",&port);
	port_number = port;

/*   Get own process id  */
	mypid = getpid();

/*   Get shared memory id for the key
     Set operation permisions and size  */
	opperm_flags = ( IPC_CREAT | 00600 );
	size = 32760 * NUMRECINP + NSPEC*4 +400;

/*   Ignore SIGUSR1                                                 */
	signal(SIGUSR1,SIG_IGN);

/*   Create shared memory  */
	shmid = shmget( key, size, opperm_flags );

     	if ( shmid == -1 ) {
		perror("[INREADER]: shm_get failed.  Exiting...\n");
		exit(-2);
	}

/*   Attach it  */
	addr = 0x0;
	rtrn_adr = (unsigned long)shmat(shmid, (void *)addr, SHM_RND );
	if ( rtrn_adr == -1 ) {
		perror("[INREADER]: shm_atch failed.  Exiting...\n");
		exit(-3);
	} 

/*   Initilaize buffer with shm address                              */
	buffer   = (int *)(rtrn_adr);

/*   Put pid in word 4  */
	*(buffer+3)  = mypid;

/*   Initially buffer is ready to accept event from inreader         */
        *(buffer+1)  = 1;

/*   Initialize word 1 of the buffer to indicate shm buffer not ready*/
	*buffer      = 2; 

/*   Initialize the communications                                   */

/*   Get the server internet address                                 */
	host_info = gethostbyname ( argv[3] );
	bcopy ( host_info->h_addr_list[0], (char *) &myaddr,
	host_info->h_length);
 
	host_address = inet_ntoa ( myaddr );
	inet_as      = inet_addr ( host_address );

	t_count = 0;
/*   Create a socket                                                 */
wait_cre:
	if ( (socket_descr = socket (af, type, protocol)) < 0 ) {
		perror("[INREADER]: Error creating a socket\n");
		if(t_count < 3) {
			t_count++;
                        sleep(40);
			goto wait_cre;
		}
		else
			goto end_of_data;
	}

/*   And connect it                                                  */
	bzero (&conn_addr, sizeof (conn_addr)) ;
	conn_addr.sin_family      = AF_INET ;
	conn_addr.sin_port        = htons (port_number) ;
	conn_addr.sin_addr.s_addr = htonl (inet_as) ;
	if (connect (socket_descr, (struct sockaddr*)&conn_addr,
                     sizeof (conn_addr)) != 0 ) {
		perror("[INREADER]: Error connecting a socket\n");
		goto end_of_data;
	}

next_event:  /* loop over events starts here */

/*   Wait for signal from reco  */
	if ( *(buffer+1) != 1 ) {
		signal(SIGUSR1,onintr_inr);
		if ( sleep(120) == 0 ) {
/* Check that RECO is still there                                    */
			if ( kill(ppid,0) == 0 ) {
				shmctl(shmid, IPC_RMID, NULL);
				goto next_event;
			}
			goto end_of_data;
		}
		signal(SIGUSR1,SIG_IGN);
		if ( *(buffer+2) == -1 )
			goto end_of_data;
  
	}

/*   Here comes the networking part  */
/*   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  */
/*                         Event request                             */
	event_rqst[0] = 3;
	if (write (socket_descr, event_rqst, BYTES) != BYTES) {
		printf("[INREADER]: Error sending event request\n");
		event_rqst[1] = -1;
		goto send_sig;
	}

/*                  Confirmation of event request                     */
        bptr = &event_rqst[0];
        if (read_buf (socket_descr, bptr, BYTES) != BYTES ) {
		printf("[INREADER]: Error getting event conf. \n");
		event_rqst[1] = -1;
		goto send_sig;
	}


/*                  In case of End-of-Data, exit                      */
        if (event_rqst[0] == 7) {
		printf("[INREADER]: End-of-data encountered\n");
		event_rqst[1] = -1;
		goto send_sig;
	}

/*                  Get number of blocks to transfer                  */
        numrec = event_rqst[1];
        numaux = 0;
        keyaux = key;
        if( numrec > NUMRECINP ) {
		numaux = numrec;
		numrec = NUMRECINP;
	}

/* Read to the main memory  */
	record_rqst[0] = 5;
	if ( write(socket_descr,record_rqst,BYTES) != BYTES ) {
		printf("[INREADER] Write record error\n");
		event_rqst[1] = -1;
		goto send_sig;
	}
	bytr = 4 * PRECL * numrec;
        bptr = buffer+NSPEC;
	if (read_buf(socket_descr,bptr,bytr) != bytr) {
		printf ("[INREADER] Read buffer error\n");
		event_rqst[1] = -1;
		goto send_sig;
	}
        while ( numaux > 0 ) {
		numaux -= NUMRECINP;
		if ( numaux > NUMRECINP )
			numrec = NUMRECINP;
		else {
			numrec = numaux;
			numaux = 0;
		}
			
		gt_count = 0;
try_get:
		gt_count++;

/* Get a new shared memory */
		keyaux = keyaux + 1;
		size = 32760 * numrec + 400;
		shmidaux = shmget( keyaux, size, opperm_flags );
     		if ( shmid == -1 ) {
/* Check maybe it is still there					*/
			if ( gt_count <= 3 ) {
				shmidaux = shmget ( keyaux, 0, opperm_flags);
				if ( shmidaux != -1 ) {
                			shmctl(shmidaux, IPC_RMID, NULL);
					goto try_get;
				}
			}
			printf("[INREADER]: Aux shm_get failed.  Exiting...\n");
			event_rqst[1] = -1;
			goto send_sig;
		}
		at_count = 0;
try_at:
		at_count++;
		addr = 0x0;
		rtrn_adr = (unsigned long)shmat(shmidaux, (void *)addr, SHM_RND );
		if ( rtrn_adr == -1 ) { 
			perror("[INREADER]: Aux shm_atch failed with \n");
			if ( at_count <= 3 ) {
				sleep(10);
				goto try_at;
			}
			perror("[INREADER]: Aux shm_atch failed.  Exiting...\n");
			printf("[INREADER]: The keyaux was %d\n",keyaux);
			event_rqst[1] = -1;
			goto send_sig;
  
		} 
/*   Initialize buffer with shm address                              */
		buffer_aux = (int *)(rtrn_adr);

/*   Read to the aux memory                                          */
		if ( write(socket_descr,record_rqst,BYTES) != BYTES ) {
			printf("[INREADER] Write record error\n");
			event_rqst[1] = -1;
			goto send_sig;
		}
		bytr = 4 * PRECL * numrec;
        	bptr = buffer_aux;
		if (read_buf(socket_descr,bptr,bytr) != bytr) {
			printf ("[INREADER] Read buffer error\n");
			event_rqst[1] = -1;
			goto send_sig;
		}

/* Detach the auxiliary buffer                                        */
		shmdt(buffer_aux);

	}
/*   End of the networking part      */

send_sig:
/*   Copy the event_rqst to the buffer  */
	bptr = buffer + 10;
	for ( i = 0; i < (NSPEC - 10); i++ ) {
		*bptr = event_rqst[i]; 
		bptr++;
	}
     
/*   Set the third word *(buffer+2) to number of records            */
	*(buffer+2) = event_rqst[1];

/*   Set the second word to 2 to prevent myself from writing again  */
	*(buffer+1) = 2;

/*   Set the first word to tell RECO that it can read event  */
        *buffer     = 1;
	if ( keyaux != key ) 
		*buffer = 4;

/*   Send signal to RECO that the buffer(s) is ready */
	kill(ppid, SIGUSR1);  

/*                In case of End-of-Data, exit                       */
        if (event_rqst[0] == 7 || event_rqst[1] == -1)
                goto end_of_data;

/* Back to the beginning of the loop */
	goto next_event;
     
end_of_data:

/* Shutdown the socket                                               */
	if(shutdown(socket_descr, 2) != 0)
		printf("[INREADER]: Error shutting the socket\n");

/* Close the socket                                                  */
	if(close(socket_descr) != 0)
		printf("[INREADER]: Error closing the socket\n");

/*    Remove the shared memory                                          */
/*              shmctl(shmid, IPC_RMID, NULL);                          */

	printf("Inreader is done.\n");
}

void onintr_inr(int x)
{}


int read_buf(int sock_des, int *buffer, int bytes)
{
	struct timeval timeout;
	fd_set rd;
	int n_des;
 	int n_found;
	int nleft;
	int count;
	int nread =0;
	char *ptr;

	FD_ZERO(&rd);

	nleft = bytes;
	ptr   = (char *)buffer;

	timeout.tv_sec  = 5;
	timeout.tv_usec = 0;

	n_des = sock_des + 1;
sel_again:
	FD_SET(sock_des, &rd);
	n_found = select (n_des, &rd, (fd_set *) 0, (fd_set *) 0, &timeout); 
	if (n_found < 0)  
		return(-errno);
	else
		if (n_found == 0 ) {
			printf("[INREADER] 5 sec read timeout\n");
			goto sel_again;
		}

	while(nleft > 0) {
		if((nread = read(sock_des, ptr, nleft)) <= 0) {
			perror("Error reading:\n");
			return(nread);
		}
		nleft -= nread;
		ptr   += nread;
	}
	return(bytes-nleft);
}
