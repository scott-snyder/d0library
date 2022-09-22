      SUBROUTINE EZGET_larr (PARAM1,IVAL,IER)
      IMPLICIT NONE
      CHARACTER*(*) PARAM1
      logical       IVAL(*)
      INTEGER       IER
      call ezget (param1, ival, ier)
      return
      end
