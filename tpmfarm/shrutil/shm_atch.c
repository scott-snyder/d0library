#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  shm_atch.c 

   Purpose:  This call will attach a shmid 
   
   Returns:  Value returned from shmat call
 
   Fortran calling sequence:

              rtrn_adr = shm_atch_(shmid)

                      Created 8/93 by Mark Galli

******************************************************************************/


 unsigned long shm_atch_( int shmid )
 {
    
    unsigned long addr, rtrn;

    addr = 0x0;

    rtrn = (unsigned long)shmat(shmid, (void *)addr, SHM_RND );
	if ( rtrn == -1 )
		perror("Attach failed");
    return(rtrn); 
 } 
