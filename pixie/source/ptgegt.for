      FUNCTION PTGEGT(ILAY,IWIR,ANODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives enrg in a TRD hit for a given layer ILAY 
C-              and a given cell IWIR, using the common blocks filled in
C-              PTEGFL subrourtine.
C-
C-   Inputs  : ILAY,IWIR - Layer and wire 
C-             ANODES    - Anodes= 1, cathodes strip=2
C-
C-   Outputs : Energy in wire IWIR layer ILAY
C-
C-   Created   3-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$INC:TRHITW.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
C----------------------------------------------------------------------
      REAL PTGEGT,TEMP,TEMP2
      INTEGER I,J,IWIR,ILAY,ANODE
C----------------------------------------------------------------------
      TEMP=0.0
      I=NBTHIT(ILAY,ANODE)
      DO 20, J=1, I 
        IF (NUMTWH(J,ILAY,ANODE).EQ.IWIR) THEN
          TEMP=ENTWH(J,ILAY,ANODE)
          GO TO 999
        ENDIF
   20 CONTINUE        
  999 PTGEGT=TEMP
      RETURN
      END
