#include "smg.h"

void smg_bell(char *string)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_bell
C-
C-   Purpose: Test a zero-terminated string for embedded control-g's.
C-            The terminal bell is rung once for each control-g.  The 
C-            control-g's are deleted from the string in situ.  Also,
C-            non-printing characters (that is, characters outside any
C-            of the ranges 8-13, 27, 32-126) are replaced by spaces.
C-
C-   Returned value: none
C-
C-   Usage: 
C-
C-      smg_bell(string);
C-
C-   Arguments: 
C-
C-      string (null-terminated string, modify) - Character string.
C-
C-   Created   30-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  int i,j;

  i = 0;
  j = 0;
  do {
    if(string[i] != '\0' &&
       (string[i] < '\007' || string[i] > '\015') && 
       string[i] != '\033' &&
       (string[i] <'\040' || string[i] > '\176') ) string[i] = ' ';
    if(string[i] == '\007')
      beep();
    else
      string[j++] = string[i];
  }
  while (string[i++] != '\0');
  return;
}
