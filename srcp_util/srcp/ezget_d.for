      SUBROUTINE EZGET_d (PARAM1,dVAL,IER)
      IMPLICIT NONE
      CHARACTER*(*) PARAM1
      real*8        dVAL(*)
      INTEGER       IER
      call ezget (param1, dval, ier)
      return
      end
