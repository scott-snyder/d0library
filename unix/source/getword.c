/*
C-----------------------------------------------------------------------
C-
C- Name      : getword
C-
C- Purpuse   : This function reads one whitespace- or null-delimited word 
C-             from standard input.  Leading and trailing whitespace and/or
C-             nulls are removed.  The word buffer is filled with s 
C-             null-delimited string. 
C-             
C- Return value:  Pointer to word buffer or NULL if EOF.
C-
C- Arguments:
C-
C-     word - Pointer to word buffer.
C-     n    - Size of buffer.
C-
C- Created 4-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/

#include <ctype.h>
#include <stdio.h>
#include "unix.h"

char *getword(char *word, int n)
{
  int c;      /* Temporary character variable. */
  int i=0;    /* Number of characters stored in word so far. */
  static int eofflag=0;
  

/* Character reading loop. */

  while(i<n) {

    if(eofflag)
      c = EOF;
    else
      c = getchar();

  /* Check for EOF.  Return NULL if no characters have been stored yet. */

    if(c == EOF) {
      eofflag = 1;
      word[i] = '\0';
      if(i == 0)
	return NULL;
      else
        return word;
    }

  /* Check for whitespace or null. */

    if(isspace(c) || c == '\0') {

    /* Skip leading whitespace/null. */

      if(i == 0)
	continue;

    /* Non-leading whitespace/null terminates word. */

      word[i] = '\0';
      return
	word;
    }

  /* Got a good character.  Store in word. */

    word[i++] = c;
  }

/* Filled up buffer if we fall out of loop here. */

  return
    word;
}
