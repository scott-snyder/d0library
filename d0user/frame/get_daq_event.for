      SUBROUTINE GET_DAQ_EVENT(IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Dummy version, prevent unsatisfied external offline
C-
C-   Created   6-FEB-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOS
C----------------------------------------------------------------------
      IOS=0
      CALL ERRMSG('DUMMY VERSION','GET_DAQ_EVENT',
     &  ' No data read from Shared Common','W')
  999 RETURN
      END
