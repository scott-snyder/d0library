      SUBROUTINE BKSAMH(IPLANE,LSAMH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book 'SAMH' bank.
C-
C-   Inputs  : 
C-      IPLANE  I   plane number from 1-18
C-
C-   Outputs : 
C-      LSAMH   I   address of bank, SAMH.
C-
C-   Created  12-MAR-1991   O.Eroshin
C-   Updated  11-SEP-1991   Daria Zieminska  remove the call to SAMHFL; 
C-   eliminate arguments
C-            10/94  MF use arguments and book single SAMH bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IPLANE
      INTEGER LSAHH,LSAMH,MMBK(5) 
      LOGICAL FIRST
      INTEGER  GZSAHH,N_SAMH,N_PLANE
      PARAMETER (N_SAMH=15,N_PLANE=18)
      SAVE FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
         CALL UCTOH('SAMH',MMBK(1),4,4)           ! IDH (hollerith bank name)
         MMBK(2) = 1                              ! NL (total number of links)
         MMBK(3) = 1                              ! NS (number of struct. links)
         CALL MZFORM('SAMH','/1B 2I 1F 1I 10F',MMBK(5)) ! NIO (bank format)
         FIRST   = .FALSE.
      ENDIF
C
      LSAMH = 0
      IF (IPLANE.LT.1.OR.IPLANE.GT.N_PLANE) RETURN
C
C  -- Get support bank SAHH
C
      LSAHH = GZSAHH() 
      IF(LSAHH.EQ.0) THEN
        CALL BKSAHH(0,0,LSAHH)
      ENDIF
      IF (LSAHH.EQ.0) RETURN
C
C  -- Book bank
C
      MMBK(4) = N_SAMH * IQ(LSAHH+IPLANE)
      IF (MMBK(4).GT.0) CALL MZLIFT(IXMAIN,LSAMH,LSAHH,-IPLANE,MMBK,0)
C
  999 RETURN
      END
