#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  shm_del.c 

   Purpose:  This call will delete the shared memory segment associated
             with the shmid
   
   Returns:  0 if successful.  -1 if unsuccessful.
 
   Fortran calling sequence:

              shmid = shm_del_(shmid)

                      Created 8/93 by Mark Galli

******************************************************************************/


int shm_del_( int shmid )
 {
    int i;
/*  remove shmem  */
    i =  shmctl(shmid, IPC_RMID, NULL);
    return(i); 
 } 
