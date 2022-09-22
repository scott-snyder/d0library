      subroutine EZSET_iarr (PARAM1,IVAL,IER)
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      integer       IVAL(*)
      INTEGER       IER

      call ezset (param1, ival, ier)
      return
      end
      
