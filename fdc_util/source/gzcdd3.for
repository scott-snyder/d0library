C VAX/DEC CMS REPLACEMENT HISTORY, Element GZCDD3.FOR
C *1     9-NOV-1993 18:01:05 AVERY "fdc changes for v12 reco"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GZCDD3.FOR
      FUNCTION GZCDD3()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank CDD3 
C-
C-   Returned value  : pointer to Zebra bank CDD3 
C-
C-   Created   5-NOV-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZCDD3
C----------------------------------------------------------------------
      GZCDD3 = 0
      IF (LHEAD .GT. 0) GZCDD3 = LQ(LHEAD - IZCDD3)
C----------------------------------------------------------------------
  999 RETURN
      END
