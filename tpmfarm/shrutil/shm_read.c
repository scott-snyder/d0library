#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  shm_read.c 

   Purpose:  To read a shared memory segment into a local buffer
   
   Returns:  None
 
   Fortran calling sequence:

     call shm_read( %val(adr), buf, %val(precl), %val(offset), %val(nspec) )

                      Created 8/93 by Mark Galli

******************************************************************************/


 void shm_read_( int addr,  int buf_in, int precl, int offset, int nspec )
 {
    
    int i;
    int *buf_shar;
    int *buf_priv;

    buf_shar = (int *)(addr);
    buf_priv = (int *)(buf_in);

/*  Point buffer to place to start writing :  
     --  this place is number of special words
     --  plus offset plus 3 off of rtrn addr.    */
    buf_shar = buf_shar + precl * offset + nspec;


/*  Now fill the buffer  */

    for ( i=0; i<precl; i++ ) {
          *buf_priv = *buf_shar;
          buf_shar++; buf_priv++;
    }
 } 
