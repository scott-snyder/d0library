#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

/*****************************************************************************

      Name:  get_word.c 

   Purpose:  This call will put a given value into a g iven word in a buffer
             at a given address 
   
   Returns:  Value in specified fullword boundry
 
   Fortran calling sequence:

              value = get_word_( word, buf @ )
                      value - integer code value contained in the word
                       word - is an integer which represents which word;
                       addr - returned address of buffer location

                      Created 8/93 by Mark Galli

******************************************************************************/


 int get_word_( unsigned long addr, int word )
 {
    
    int *buffer;
    int value;

    buffer = (int *)addr;
    buffer = buffer + (word-1);
    value = *buffer;
    return(value);

 } 
