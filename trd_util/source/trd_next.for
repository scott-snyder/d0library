      FUNCTION TRD_NEXT(I,LAYER) 
C----------------------------------------------------------------------
C-   Purpose and Methods :  Computes i-1 wire
C-
C-   Inputs  : I (integer) wire number
C-             LAYER (integer) TRD layer (1,2,3)
C-   Outputs : TRD_NEXT in [1,256] or [1,512]
C-   Controls: none
C-
C-   Created   8-JUN-1993   Alain PLUQUET
C-   Updated  15-OCT-1993   Alain PLUQUET  adds 512 wire case 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,LAYER,TRD_NEXT,NUMBER_OF_WIRES(6)
      CALL GET_TRD_NBW(NUMBER_OF_WIRES)
      IF (I.NE.NUMBER_OF_WIRES(LAYER)) THEN
        TRD_NEXT=I+1
      ELSE
        TRD_NEXT=1
      ENDIF
      END
