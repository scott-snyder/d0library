      SUBROUTINE FGETLDR2(ITRACK,LADDER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the segment ladder for a given FDC
C-                         track as stored in the FDTH bank.
C-
C-   Inputs  : ITRACK
C-   Outputs : LADDER(0:2)
C-
C-   Created  24-AUG-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ITRACK,LADDER(0:2),LOC
      INTEGER GZFDCT
C----------------------------------------------------------------------
      CALL VZERO(LADDER(0),3)
      LOC=GZFDCT(ITRACK)
      IF(LOC.LE.5) GOTO 999
      LOC=LQ(LOC-1)
      IF(LOC.LE.5) GOTO 999
      IF(IQ(LOC-1).EQ.105) THEN
        LADDER(0) = IQ(LOC+103)
        LADDER(1) = IQ(LOC+104)
        LADDER(2) = IQ(LOC+105)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
