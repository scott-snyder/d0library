#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <strings.h>
#include <time.h>
#include "pfarms.h"
/*******************************************************************************

                       Name:  outwriter.c

Kirill Denisenko, 8/17/93

          Calling sequence:  outwriter.x  key_out, pid, node, port
                             Where key_out is the key to be associated
                             with the outwriter's shared memory segment,
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
	int opperm_flags_create, opperm_flags_exist;
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
	int new_connection;

        time_t cur_time;
	int t_count, pidchild;
	int sig;
	int numrec, numaux;
	int keyaux, blkfactor;
	int *bptr, bytread, bytwrite;
	unsigned bytr;

/*  Function Prototypes                                            */
	int read_buf(int , int *, int);
	void onintr_owr(int);
	/*void bzero(void *, int );*/
	/*void bcopy(const void *, void *, int);*/

/*   Check number of arguments  */
	if ( argc != 5 ) {
		printf("[OUTWRITER]: Wrong calling sequence.  Exiting...\n"); 
		exit(-1);
     	}

/*   Convert key and pid to integers  */
	sscanf(argv[1],"%d",&key);
	sscanf(argv[2],"%d",&piden);
	ppid  = piden;

/*   Convert port number to integer   */
	sscanf(argv[4],"%u",&port);
	port_number = port;

/* Now make two copies of it - one for STAs and one for DSTs         */
	blkfactor = NUMRECSTA;
	sig = SIGUSR2;
	if ( (pidchild = fork()) == -1 ) {
		printf("[OUTWRITER] Fork failed, exiting...");
		exit(-2);
	} 
	if ( pidchild == 0 ) {
		blkfactor = NUMRECDST;
		key = key + 100;
		sig = SIGINT;
	}

/*   Get own process id  */
	mypid = getpid();

/*   Get shared memory id for the key
     Set operation permisions and size  */
	opperm_flags_create = ( IPC_CREAT | 00600 );
	opperm_flags_exist  = 00600;
	size = 32760 * blkfactor + NSPEC * 4 + 400;

/*   Ignore sig                                                 */
	signal(sig,SIG_IGN);

/*   Create shared memory  */
	shmid = shmget( key, size, opperm_flags_create );

     	if ( shmid == -1 ) {
		printf("[OUTWRITER]: shm_get failed.  Exiting...\n");
		exit(-2);
	}

/*   Attach it  */
	addr = 0x0;
	rtrn_adr = (unsigned long)shmat(shmid, (void *)addr, SHM_RND );
	if ( rtrn_adr == -1 ) {
		printf("[OUTWRITER]: shm_atch failed.  Exiting...\n");
		exit(-3);
	} 

/*   Initilaize buffer with shm address                              */
	buffer   = (int *)(rtrn_adr);

/*   Put pid in word 4  */
	*(buffer+3)  = mypid;

/*   Initially buffer is not ready to get it from RECO               */
        *(buffer+1)  = 2;

/*   Initialize word 1 of the buffer to indicate buffer is RECO ready*/
	*buffer      = 1; 

/*   Initialize the communications                                   */

/*   Get the server internet address                                 */
	host_info = gethostbyname ( argv[3] );
	bcopy ( host_info->h_addr_list[0], (char *) &myaddr,
	host_info->h_length);
 
	host_address = inet_ntoa ( myaddr );
	inet_as      = inet_addr ( host_address );

/* Reset the flag for re-connecting the socket
	new_connection = 0;


/*   Create a socket                                                 */
	t_count = 0;
reconnect:
	if ( (socket_descr = socket (af, type, protocol)) < 0 ) {
		printf("[OUTWRITER]: Error creating a socket: %d\n", errno);
		goto end_of_data;
	}

/*   And connect it                                                  */
	bzero (&conn_addr, sizeof (conn_addr)) ;
	conn_addr.sin_family      = AF_INET ;
	conn_addr.sin_port        = htons (port_number) ;
	conn_addr.sin_addr.s_addr = htonl (inet_as) ;

	if (connect (socket_descr, (struct sockaddr*)&conn_addr,
                     sizeof (conn_addr)) != 0 ) {
		printf("[OUTWRITER]: Error connecting a socket: %d\n", errno);
		if ( t_count < 3 ) {
			t_count++;
			sleep(20);
			if(close(socket_descr) != 0)
				printf("[OUTWRITER]: Error closing the socket: %d\n", errno);
			goto reconnect;
		}
		else
			goto end_of_data;
	}

	printf("[OUTWRITER] Initial Buffer[1]=%d\n",*(buffer+1));

/* If it is just reconnecting a socket, get back to the event_rqst part */
	if(new_connection == 1) {
		printf("[OUTWRITER]: Successfully reconnected the socket\n");
		goto event_req;
	}

next_event:  /* loop over events starts here */

/*   Wait for signal from reco  */
        if ( *(buffer+1) != 1 ) {
                signal(sig,onintr_owr);
                if ( sleep(120) == 0 ) {
/* Check that RECO is still there                                    */
                        if ( kill(ppid,0) != 0 ) {
				printf("[OUTWRITER] No RECO running\n");
                                goto end_of_data;
			};
                        goto next_event;
                }
                signal(sig,SIG_IGN);
		if ( *(buffer+2) == -1 ) {
			printf("Reco send an abort\n");
			goto end_of_data;
		}
        }

/*   Copy the event_rqst from the buffer                             */
	bptr = buffer + 10;
	for ( i = 0; i < NSPEC-10; i++ ) {
		event_rqst[i] = *bptr; 
		bptr++;
	}

/* If event request == 0 - print pidchild end exit                   */
	if ( event_rqst[0] <= 0 ) {
		printf("[OUTWRITER] Wrong event_rqst from shm, pid %d\n",
		pidchild);
		printf("[OUTWRITER] event_request[1]=%d\n",event_rqst[0]);
		goto end_of_data;
	}

/*                  Get number of blocks to transfer                  */
        numrec = *(buffer+2);
        if(numrec == 0) {
		printf("[OUTWRITER] Zero number of records in event\n");
		goto skip_event;
	};
        numaux = 0;
        keyaux = key;
        if( numrec > blkfactor ) {
		numaux = numrec;
		numrec = blkfactor;
	}

/*   Here comes the networking part  */

event_req:
	new_connection = 0;

/*                         Event request                             */
	if (write (socket_descr, event_rqst, BYTES) != BYTES) {
		printf("[OUTWRITER]: Error sending event request\n");
		goto renegotiate;
	}


/*   This is a termination record; stop the outwriter                 */
	if ( event_rqst[0] == 9 ) {
		printf("[OUTWRITER] RECO sent termination (9)\n");
		goto end_of_data;
	};

/*                  Confirmation of event request                     */
        bptr = &event_rqst[0];
        if (( bytread = read_buf (socket_descr, bptr, BYTES)) != BYTES ) {
		printf("[OUTWRITER]: Error getting event conf. \n");
		printf("[OUTWRITER]: Bytes read %d\n",bytread);
		printf("[OUTWRITER]: Event_request[1] = %d\n", *bptr);

/* Shutdown the socket                                                  */
renegotiate:
		cur_time = time ( ( time_t * ) 0);
		printf("[OUTWRITER]: Time %s",ctime(&cur_time));

		if(shutdown(socket_descr, 2) != 0)
			printf("[OUTWRITER]: Error shutting the socket\n");

/* Close the socket                                                     */
		if(close(socket_descr) != 0)
			printf("[OUTWRITER]: Error closing the socket\n");

		new_connection = 1;
		goto reconnect;
	}

/* Prepare a record request header */
	record_rqst[1] = numrec;
	record_rqst[0] = 5;
	if ( numaux == 0 ) 
		record_rqst[0] = 7;

/* Write from the main memory  */
	if ( write(socket_descr,record_rqst,BYTES) != BYTES ) {
		printf("[OUTWRITER] Write record error\n");
		goto end_of_data;
	}

	if ((bytread = read_buf(socket_descr,record_rqst,BYTES)) != BYTES) {
		printf ("[OUTWRITER] Read record conf. error\n");
		printf("[OUTWRITER]: Bytes read %d\n",bytread);
		printf ("[OUTWRITER]: Pidchild = %d\n", pidchild);
		printf("[OUTWRITER]: Record_request[1] = %d\n", 
		record_rqst[0]);
		printf("[OUTWRITER]: Record_request[2] = %d\n", 
		record_rqst[1]);
		goto end_of_data;
	}

	bytr = 4 * (PRECL + 1) * numrec;
        bptr = buffer+NSPEC;
	if ((bytwrite = write(socket_descr,bptr,bytr)) != bytr) {
		printf ("[OUTWRITER] Write buffer error\n");
		printf ("[OUTWRITER] Pidchild = %d\n", pidchild);
		printf ("[OUTWRITER] Bytes to write %d\n", bytr);
		printf ("[OUTWRITER] Bytes written %d\n", bytwrite);
		goto end_of_data;
	}
	if (read_buf(socket_descr,record_rqst,BYTES) != BYTES) {
		printf ("[OUTWRITER] Read buffer conf. error\n");
		goto end_of_data;
	}
        while ( numaux > 0 ) {
		numaux -= blkfactor;
		if ( numaux > blkfactor )
			numrec = blkfactor;
		else {
			numrec = numaux;
			numaux = 0;
		}
			
/* Get a new shared memory */
		keyaux = keyaux + 1;
		size = 32760 * numrec + 400;
		shmidaux = shmget( keyaux, size, opperm_flags_exist );
     		if ( shmid == -1 ) {
			printf("[OUTWRITER]: Aux shm_get failed.  Exiting...\n");
			goto end_of_data;
		}
		addr = 0x0;
		t_count = 0;
wait_atch:
		rtrn_adr = (unsigned long)shmat(shmidaux, (void *)addr, SHM_RND );
		if ( rtrn_adr == -1 ) { 
			printf("[OUTWRITER]: Aux shm_atch failed.  Exiting...\n");
                	if ( t_count < 3 ) {
                       		t_count++;
                        	sleep(40);
                       		goto wait_atch;
                	}
                	else
				goto end_of_data;
		} 

/*   Initialize buffer with shm address                                */
		buffer_aux = (int *)(rtrn_adr);

/*   Write from the aux memory                                          */
		record_rqst[1] = numrec;
		record_rqst[0] = 5;
		if ( numaux == 0 ) 
			record_rqst[0] = 7;
		if ( write(socket_descr,record_rqst,BYTES) != BYTES ) {
			printf("[OUTWRITER] Write record rqst. error\n");
			goto end_of_data;
		}

		if (read_buf(socket_descr,record_rqst,BYTES) != BYTES) {
			printf ("[OUTWRITER] Read record conf. error\n");
			goto end_of_data;
		}

                bytr = 4 * (PRECL + 1) * numrec;
        	bptr = buffer_aux;
		if ((bytwrite=write(socket_descr,bptr,bytr)) != bytr) {
			printf ("[OUTWRITER] Write aux buffer error\n");
                  	printf ("[OUTWRITER] Bytes to write %d\n", bytr);
                        printf ("[OUTWRITER] Bytes written %d\n", bytwrite);
			goto end_of_data;
		}

		if (read_buf(socket_descr,record_rqst,BYTES) != BYTES) {
			printf ("[OUTWRITER] Read buffer conf. error\n");
			goto end_of_data;
		}

/*    Detach the shared memory                                          */
		shmdt(buffer_aux);

/*    Remove the shared memory                                          */
		shmctl(shmidaux, IPC_RMID, NULL);

	}


/*   End of the networking part      */
skip_event:

/*   Set the second word to 2 to prevent myself from writing again      */
	*(buffer+1) = 2;

/*   Set the first word to tell RECO that it can write the next event   */
        *buffer     = 1;

/*   Send signal to RECO that the buffer(s) is ready                    */
	kill(ppid, sig);  

/* Back to the beginning of the loop                                    */
	goto next_event;
     
end_of_data:

/* Print the current time */
	cur_time = time ( ( time_t * ) 0);
	printf("[OUTWRITER]: Time %s",ctime(&cur_time));

/* Shutdown the socket                                                  */
	if(shutdown(socket_descr, 2) != 0)
		printf("[OUTWRITER]: Error shutting the socket\n");

/* Close the socket                                                     */
	if(close(socket_descr) != 0)
		printf("[OUTWRITER]: Error closing the socket\n");

/*    Remove the shared memory                                          */
		shmctl(shmid, IPC_RMID, NULL);

	cur_time = time ( ( time_t * ) 0);
	printf("[OUTWRITER]: Time %s",ctime(&cur_time));
	printf("OUTWRITER is done.\n");
}

void onintr_owr(int x)
{}

int read_buf(int sock_des, int *buffer, int bytes)
{
	struct timeval timeout;
	fd_set rd, ex;
	int n_des;
 	int n_found;
	int nleft;
	int nread =0;
	char *ptr;


	nleft = bytes;
	ptr   = (char *)buffer;

	timeout.tv_sec  = 100;
	timeout.tv_usec = 0;

	n_des = sock_des + 1;
	FD_ZERO(&rd);
	FD_ZERO(&ex);
	FD_SET(sock_des, &rd);
	FD_SET(sock_des, &ex);
	n_found = select (n_des, &rd, (fd_set *) 0, &ex, &timeout);
	if (n_found <=0)  
		return(n_found);
	while(nleft > 0) {
		if((nread = read(sock_des, ptr, nleft)) <= 0) {
			printf("[OUTWRITER] Error reading %d %d %d\n", nread,
			nleft);
			printf("[OUTWRITER] Errno = %d\n",errno);
			return(nread);
		}
		nleft -= nread;
		ptr   += nread;
	}
	return(bytes-nleft);
}
