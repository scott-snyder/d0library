      SUBROUTINE INZLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Initialize link area /ZLINKA/
C-
C-   Created  28-OCT-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZLINK(IXCOM,'/ZLINKA/',LSLINK,LRLINK,LRLINK(NRLINK))
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
