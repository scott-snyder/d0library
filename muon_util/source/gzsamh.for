      INTEGER FUNCTION GZSAMH(NST,NPL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK SAMH
CC
CC    O.Eroshin 10-7-86
CC    call GZSAHH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSAMH.LINK'
      INTEGER NST,NPL,GZSAHH,LSAHH
C
      LSAHH    = GZSAHH()  
      IF(LSAHH.NE.0) THEN
        GZSAMH = LQ(LSAHH-3*(NST-1)-NPL) 
                     ELSE
        GZSAMH = 0
      ENDIF
      RETURN
      END
