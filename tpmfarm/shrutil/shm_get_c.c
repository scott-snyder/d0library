#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

/*****************************************************************************

      Name:  shm_get_c.c 

   Purpose:  This call will make the system call shmget with 
             operation permission IPC_CREATE ORed with read
             and write permisions.  A default shared memory
             size of 6 kb is used.    
   
   Returns:  shmid is returned if successful.  If not, the
             same error # returned from shmget call is returned
 
   Fortran calling sequence:

              shmid = shm_get_c(key, numrec, offset)

              -- Where 'key_in' is the key to be associated with 
              and size is the size in bytes the shared memory
              should be.

              -- numrec is the number of records to keep in the
              units of 32760 byte records

              -- number of additional words to add to the size

                      Created 8/93 by Mark Galli
                      Modified 8/13/93 Kirill Denisenko ( 2 args added )

******************************************************************************/


int shm_get_c_( int key_cr, int numrec, int spcoffset )
 {

    key_t key;
    int opperm_flags;
    int shmid, size;

    key = key_cr;

/*  Set operation permisions and size  */
    opperm_flags = ( IPC_CREAT | 00600 ); 
    size = 32760 * numrec + spcoffset*4 + 400;

/*  Create shared memory  */
    shmid = shmget( key, size, opperm_flags );

/*  Return identifier  */
    return(shmid);
 } 
