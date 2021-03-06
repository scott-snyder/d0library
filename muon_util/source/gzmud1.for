C DEC/CMS REPLACEMENT HISTORY, Element GZMUD1.FOR
C *2    10-MAR-1988 15:32:56 TAMI "Fix to check for LHEAD=0"
C *1     9-DEC-1986 17:12:10 HEDIN "Finds pointer to muon Zebra bank"
C DEC/CMS REPLACEMENT HISTORY, Element GZMUD1.FOR
      INTEGER FUNCTION GZMUD1(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK MUD1
CC
CC    HEDIN 10-7-86
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
      INTEGER I
      GZMUD1 = 0
      IF (LHEAD .NE. 0) THEN
         GZMUD1=LQ(LHEAD-IZMUD1)
      ENDIF
      RETURN
      END
