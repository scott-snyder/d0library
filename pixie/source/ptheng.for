      SUBROUTINE PTHENG(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets the wire and layer number of the highest 
C-                 energy release in the TRD.
C-
C-   Inputs  : WIRE,LAYER
C-   Outputs : NTFADC  NUMBER OF TOTAL  FADC HITS
C-             TLAY    ARRAY WITH LAYER NUMBER
C-             TWIR    ARRAY WITH WIRE NUMBERS 
C-             TFADC   ARRAY WITH ENERGY RELEASE PER WIRE
C-
C-   Created   17-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$INC:TRHITW.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
C----------------------------------------------------------------------
      REAL MAXENG
      INTEGER I,J,WIRE,LAYER,TOTHIT,ANODE
      DATA ANODE/1/
C----------------------------------------------------------------------
      MAXENG = 0.0
      DO 100 I=1,3
        DO 100 J=1, NBTHIT(I,ANODE) 
          IF (ENTWH(J,I,ANODE).GT.MAXENG) THEN
            MAXENG = ENTWH(J,I,ANODE)
            LAYER = I
            WIRE = INT(NUMTWH(J,I,ANODE))
          ENDIF
  100 CONTINUE
  999 RETURN
      END
