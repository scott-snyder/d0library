      SUBROUTINE GET_TRD_NBW(NUMBER_OF_WIRES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns the number of wires, independently of
C-                         job type.
C-
C-   Inputs  : none
C-   Outputs : NUMBER_OF_WIRES integer(3) number of wires in layers 1,2,3
C-   Controls: none
C-
C-   Created   7-OCT-1993   Alain PLUQUET
C-   Updated  16-MAR-1994   A. Zylberstejn  :simplify
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZTRDT,LTRDT,LTPRL
      LOGICAL RUN1A
      INTEGER NUMBER_OF_WIRES(6),LAYER
      LOGICAL FOUND
      REAL VERSION
      IF(NUMBER_OF_WIRES(1).lE.0)THEN
        NUMBER_OF_WIRES(1)=256
        NUMBER_OF_WIRES(2)=256
        NUMBER_OF_WIRES(4)=256
        NUMBER_OF_WIRES(5)=256
        NUMBER_OF_WIRES(3)=512
        IF(RUN1A())NUMBER_OF_WIRES(3)=256
      ELSE
        NUMBER_OF_WIRES(3)=512
        IF(RUN1A())NUMBER_OF_WIRES(3)=256
      END IF
      END
