      FUNCTION TRD_PREVIOUS(I,LAYER)
C----------------------------------------------------------------------
C-   Purpose and Methods :  Computes i+1 wire
C-
C-   Inputs  : I (integer) wire number
C-             LAYER (integer) TRD layer (1,2,3)
C-   Outputs : TRD_PREVIOUS in [1,256] or [1,512]
C-   Controls: none
C-
C-   Created   8-JUN-1993   Alain PLUQUET
C-   Updated  15-OCT-1993   Alain PLUQUET  adds 512 wire case 
C----------------------------------------------------------------------
      INTEGER I,LAYER,TRD_PREVIOUS,NUMBER_OF_WIRES(6)
      CALL GET_TRD_NBW(NUMBER_OF_WIRES)
      IF (I.NE.1) THEN
        TRD_PREVIOUS=I-1
      ELSE
        TRD_PREVIOUS=NUMBER_OF_WIRES(LAYER)
      ENDIF
      END
