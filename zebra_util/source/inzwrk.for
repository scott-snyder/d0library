      SUBROUTINE INZWRK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Initialize /ZEBWRK/, working area not written out
C-
C-   Created  28-OCT-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBWRK.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZSTOR (IXWRK,'/ZEBWRK/','Q',FENWRK,LWRKH,LRWRK,ZWORK(1),
     &   ZWORK(10000),ENDZW)
        IDVWRK=IXWRK+2
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
