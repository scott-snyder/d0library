#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  shm_write.c 

   Purpose:  To wrie to shared memory
   
   Returns:  None
 
   Fortran calling sequence:

     call shm_write( %val(adr), buf, %val(precl), %val(offset), %val(nspec) )

                      Created 8/93 by Mark Galli

******************************************************************************/


 void shm_write_( unsigned long addr,  unsigned long buf_in, int precl, int offset, int nspec )
 {
    
    int i;
    int *buf_shar;
    int *buf_priv;

    buf_shar = (int *)(addr);
    buf_priv = (int *)(buf_in);

/*  Point buffer to place to start writing :  
     --  this place is the number of special words
         plus offset plus 1    */
    buf_shar = buf_shar + offset * precl + nspec;


/*  Now fill the buffer  */

    for ( i=0; i<precl; i++ ) {
          *buf_shar = *buf_priv;
          buf_shar++; buf_priv++;
    }
 } 
