      SUBROUTINE TRD_NWIRE_PER_LAYER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define nb. of channels in each TRD layer
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-JUL-1993   A. ZYLBERSTEJN
C-   Updated  21-DEC-1993   Alain PLUQUET  use RUN1A function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,N
      LOGICAL RUN1A
      NWIRE_PER_LAYER(1)=NW1
      NWIRE_PER_LAYER(2)=NW2
      NWIRE_PER_LAYER(3)=NW3
      IF(RUN1A())
     &            NWIRE_PER_LAYER(3)=256
      NWIRE_PER_LAYER(4)=NW4
      NWIRE_PER_LAYER(5)=NW5
      NWIRE_PER_LAYER(6)=NW6
      N=0
      DO I=1,6
        FIRST_WIRE(I)=N
        N=N+NWIRE_PER_LAYER(I)
      END DO
  999 RETURN
      END
