      SUBROUTINE MUISP1(DCIX,DCIY,DCIZ,PISA,DCX,DCY,DCZ,IDA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns info from the ISAJET banks for
C-       the muon which is closest to a given direction
C-   Inputs  : DCIX,DCIY,DCIZ  -----  input direction cosines
C-   Outputs : PISA   ---   ISAJET momentum (signed)
C-             DCX,DCY,DCZ  ---   ISAJET direction cosines
C-             IDA 0=PROMPT 1=DECAY 2=PUNCH
C-   Created  20-MAR-1990   David Hedin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SI
      PARAMETER (SI=100)
      INTEGER NPART,ID(SI),ID2(SI)
      REAL X(SI),Y(SI),Z(SI)
      REAL PX(SI),PY(SI),PZ(SI),P(SI),PHI(SI),THETA(SI),ETA(SI)
      INTEGER IDA,I
      REAL PISA,DCX,DCY,DCZ,DOT,DCIX,DCIY,DCIZ,DOTMAX
C----------------------------------------------------------------------
      PISA=99999.
      DCX=10000.
      DCY=10000.
      DCZ=10000.
      DOTMAX=-1.
      CALL GISAMU(NPART,ID,ID2,PX,PY,PZ,P,PHI,THETA,ETA,X,Y,Z)
CCC   FIND CLOSEST MUON
      IF(NPART.GE.0) THEN
      DO I=1,NPART
        IF(P(I).GT.1.) THEN
        DOT=(PX(I)*DCIX+PY(I)*DCIY+PZ(I)*DCIZ)/P(I)
        IF(DOT.GT.DOTMAX) THEN
          DOTMAX=DOT
          IDA=ID2(I)
          PISA=-ID(I)/IABS(ID(I))*P(I)
          DCX=PX(I)/P(I)
          DCY=PY(I)/P(I)
          DCZ=PZ(I)/P(I)
        ENDIF
       ENDIF
      ENDDO
      ENDIF
  999 RETURN
      END
