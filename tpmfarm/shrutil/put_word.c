#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  put_word.c 

   Purpose:  This call will put a given value into a g iven word in a buffer
             at a given address 
   
   Returns:  None
 
   Fortran calling sequence:

              status = put_word_( word, value, buf @ )
                        word - is an integer which represents which word;
                       value - integer code value to fill in the word
                        addr - returned address of buffer location

                      Created 8/93 by Mark Galli

******************************************************************************/


 void put_word_( int addr,  int word, int value )
 {
    
    int *buffer;

    buffer = (int *)(addr);
    buffer = buffer + (word-1);
    *buffer  = value; 

 } 
