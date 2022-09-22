      subroutine EZSET_l (PARAM1,IVAL,IER)
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      logical       IVAL(*)
      INTEGER       IER

      call ezset (param1, ival, ier)
      return
      end
      
