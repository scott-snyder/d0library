      integer function lib$signal(cond)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : "Emulate" the VMS RTL routine lib$signal
C-
C-   Returned value  : 1 (true)
C-   Inputs  : cond - Condition value (pass by value)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   15-AUG-1991   Herbert Greenlee
C-
C-   This routine exists mainly to avoid unsatisfied externals in routines
C-   that call lib$signal.  It prints an error message that includes the 
C-   condition value, which is generally not very informative.
C-
C----------------------------------------------------------------------
      implicit none
      integer cond
      integer d0_loc
C----------------------------------------------------------------------
      print 10,d0_loc(cond)
 10   format(' LIB$SIGNAL called with condition value ',i6)
      lib$signal = 1
  999 return
      end
