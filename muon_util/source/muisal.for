C DEC/CMS REPLACEMENT HISTORY, Element MUISAL.FOR
C *1    22-MAR-1990 12:46:03 HEDIN "initial"
C DEC/CMS REPLACEMENT HISTORY, Element MUISAL.FOR
      SUBROUTINE MUISAL(DCIX,DCIY,DCIZ,PISA,DCX,DCY,DCZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns info from the ISAJET banks for
C-       the muon which is closest to a given direction
C-   Inputs  : DCIX,DCIY,DCIZ  -----  input direction cosines
C-   Outputs : PISA   ---   ISAJET momentum (signed)
C-             DCX,DCY,DCZ  ---   ISAJET direction cosines
C-
C-   Created  20-MAR-1990   David Hedin
C-   DH 9/90 FLIP SIGNS
C-   DH 1/91 move go to statement
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LSUP,LISAL,ID
      REAL PISA,DCX,DCY,DCZ,P(4),PHI,TH,ETA,DOT,DCIX,DCIY,DCIZ,DOTMAX
C----------------------------------------------------------------------
      PISA=99999.
      DCX=10000.
      DCY=10000.
      DCZ=10000.
      DOTMAX=-1.
      LSUP=0
    1 CALL GTISAL(LSUP,LISAL,ID,P,PHI,TH,ETA)
      IF(LISAL.NE.0) THEN
        LSUP=LISAL
CCC   FIND CLOSEST MUON
        DOT=(P(1)*DCIX+P(2)*DCIY+P(3)*DCIZ)/P(4)
        IF(IABS(ID).EQ.14.AND.DOT.GT.DOTMAX) THEN
          DOTMAX=DOT
          PISA=-ID/IABS(ID)*P(4)
          DCX=P(1)/P(4)
          DCY=P(2)/P(4)
          DCZ=P(3)/P(4)
        ENDIF
        GO TO 1
      ENDIF
  999 RETURN
      END
