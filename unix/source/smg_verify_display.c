#include "smg.h"

DISPLAY *smg_verify_display(DISPLAY *my_display)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_verify_display
C-
C-   Purpose: Verify that the named display is in the display chain.
C-
C-   Returned value: Pointer to the named display or NULL
C-
C-   Usage:
C-
C-     display = smg_verify_display(display);
C-
C-   Arguments: 
C-
C-     display - DISPLAY pointer.
C-
C-   Created   23-AUG-1991   Herbert Greenlee
C-
C-   This c-callable functin is used internally by the smg emulataion 
C-   package.  It is used to verify that a display pointer points to a
C-   display structure that is currently in the active display chain.
C-   Its only purpose is error-checking.  Failure to locate the specified
C-   display is evidence of a bug in the smg package or in the calling 
C-   program.
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  display = smg_display_root;
  do
    if(display == my_display)
      break;
  while((display = display->next) != NULL);
  return display;
}
