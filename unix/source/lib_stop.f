      subroutine lib$stop(cond)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Emulate the VMS RTL routine lib$stop
C-
C-   Returned value  : None
C-   Inputs  : cond - Condition value (pass by value)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   15-AUG-1991   Herbert Greenlee
C-
C-   This routine prints an error message and aborts the program.
C-
C----------------------------------------------------------------------
      implicit none
      integer cond
      integer d0_loc
C----------------------------------------------------------------------
      print 10,d0_loc(cond)
 10   format(' LIB$STOP called with condition value ',i6)
      call abort
  999 stop
      end
