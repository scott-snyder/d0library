      FUNCTION CELL_FBKCAP(IETA,IPHI,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-JUL-1992   Chip Stewart
C-   Updated   5-AUG-1993   Jan Guida  Change ICD from 20pf to 22pf 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CELL_FBKCAP
      INTEGER IETA,IPHI,ILYR
C----------------------------------------------------------------------
      CELL_FBKCAP = 5
      IF(ILYR.EQ.9) THEN                                  !ICD
        CELL_FBKCAP = 22
        GOTO 999
      ELSE IF(ILYR.GT.2.AND.ILYR.LT.8) THEN               !EM FLOOR 3 & 4
        CELL_FBKCAP = 10
        GOTO 999
      ELSE IF((ILYR.EQ.11).AND.
     &  ((ABS(IETA).EQ.13).OR.(ABS(IETA).EQ.14))) THEN    !ECMH 
        CELL_FBKCAP = 10
        GOTO 999
      ELSE IF((ILYR.GT.12).AND.(ILYR.LT.16).AND.
     &  (ABS(IETA).EQ.37)) THEN                           !ECIH 
        CELL_FBKCAP = 10
        GOTO 999
      ELSE IF((ILYR.EQ.8).AND. (ABS(IETA).EQ.12)) THEN    !CCMG ETA=12
        CELL_FBKCAP = 10
      END IF
  999 RETURN
      END
