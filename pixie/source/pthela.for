      SUBROUTINE PTHELA(LAYER, ANODE, WIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds the wire number that has the highest 
C-           enery released in LAYER in the TRD 
C-
C-   Inputs  : LAYER  - Layer where we are looking for the highest energy
C-             ANODE  - Anodes=1, cathodes strip = 2
C- 
C-   Outputs : WIRE   - Wire number that has the highest energy in LAYER
C-                      ANODE.
C-
C-   Created  19-JAN-1989   LUPE ROSAS
C-   Updated  13-JUN-1989   LUPE ROSAS  Anode parameter added 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRHITW.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
C----------------------------------------------------------------------
      INTEGER WIRE,LAYER,I,TOTHIT,J,ANODE
      REAL MAXENG
C----------------------------------------------------------------------
      MAXENG = 0.0
      DO 110 J=1, NBTHIT(LAYER,ANODE) 
        IF(ENTWH(J,LAYER,ANODE).GT.MAXENG) THEN
          MAXENG = ENTWH(J,LAYER,ANODE)
          WIRE = NUMTWH(J,LAYER,ANODE)     
        ENDIF
  110 CONTINUE
  999 RETURN
      END
