      FUNCTION GZSAHH 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK SAHH
CC
CC    O.Eroshin 10-7-86
CC    call GZMUHT
C-   Updated  20-JAN-1992   Daria Zieminska  eliminate dummy argument 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER GZSAHH
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSAHH.LINK'
      INTEGER GZMUHT,LMUHT
C
      LMUHT    = GZMUHT(0)  
      IF(LMUHT.NE.0) THEN
        GZSAHH = LQ(LMUHT-IZSAHH) 
      ELSE
        GZSAHH = 0
      ENDIF
      RETURN
      END
