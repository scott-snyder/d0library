      SUBROUTINE EZGET_iarr (PARAM1,IVAL,IER)
      IMPLICIT NONE
      CHARACTER*(*) PARAM1
      integer       IVAL(*)
      INTEGER       IER
      call ezget (param1, ival, ier)
      return
      end
