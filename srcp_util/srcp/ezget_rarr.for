      SUBROUTINE EZGET_rarr (PARAM1,rVAL,IER)
      IMPLICIT NONE
      CHARACTER*(*) PARAM1
      real          rVAL(*)
      INTEGER       IER
      call ezget (param1, rval, ier)
      return
      end
