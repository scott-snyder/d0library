      INTEGER FUNCTION TWIRCOR (IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct for swap cables in cosmic run
C-
C-   Inputs  : Wire number hit
C-   Outputs : Wire number hit
C-   Controls:
C-
C-   Created  30-OCT-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IN,WIRE
      INCLUDE 'D0$INC:tcntrl.INC'
C----------------------------------------------------------------------
      WIRE=IN
      IF(COSMIC1)THEN
        IF(IN.GE.49  .AND. IN.LE.56)WIRE=IN+8
        IF(IN.GE.57  .AND. IN.LE.64) WIRE=IN-8
        IF(IN.GE.80  .AND. IN.LE.88) WIRE=IN+8
        IF(IN.GE.89  .AND. IN.LE.96) WIRE=IN-8
        IF(IN.GE.193 .AND. IN.LE.200)WIRE=IN-8
        IF(IN.GE.65  .AND. IN.LE.72) WIRE=IN+8
        IF(IN.GE.73  .AND. IN.LE.80) WIRE=IN-8
      ELSE
        IF(IN.LE.16)THEN
          IF(IN.LE.8)WIRE=IN+8
          IF(IN.GT.8)WIRE=IN-8
        END IF
      END IF
      TWIRCOR=WIRE
  999 RETURN
      END
