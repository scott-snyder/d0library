      INTEGER FUNCTION TCHNB(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives the channel number (from 1 to 1536 for 256
C-   wires in all layers; 1792 when layer 3 has 512 wires)
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-JUL-1993   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INTEGER WIRE,LAYER
      INTEGER I,N
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C      IF(FIRST)THEN
C        N=0
C        DO I=1,6
C          FIRST_WIRE(I)=N
C          N=N+NWIRE_PER_LAYER(I)
C        END DO
C        FIRST=.FALSE.
C      END IF
      TCHNB=FIRST_WIRE(LAYER)+WIRE
  999 RETURN
      END
