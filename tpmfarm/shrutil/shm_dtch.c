#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  shm_dtch.c 

   Purpose:  This call will detach a shmid 
   
   Returns:  Value returned from shmdt call
 
   Fortran calling sequence:

              rtrn_adr = shm_dtch(shmaddr)


******************************************************************************/


 int shm_dtch_( unsigned long shmaddr )
 {
    
    int rtrn;


    rtrn = (int)shmdt( (void *)shmaddr );
	if ( rtrn == -1 )
		perror("Detach failed");
    return(rtrn); 
 } 
