      SUBROUTINE STP_INZLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Initialize link area /STP_ZLINKA/
C-
C-   Created  28-SEP-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZLINK(IXSTP,'/STP_ZLINKA/',
     &    STP_LSLINK,STP_LRLINK,STP_LRLINK(STP_NRLINK))
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
