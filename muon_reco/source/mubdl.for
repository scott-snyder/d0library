      SUBROUTINE MUBDL(MOM,QUAD,PRIMAX,X,Y,Z,DX,DY,DZ,DSL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate integrated B field for a muon track
C-
C-   Inputs  : 
C-      X,Y,Z  position at magnet center (FROM B_C LAYER)
C-      DOZ, direction cosines outside
C-   Outputs : 
C-      DSL(2)  change of slope for unit mom
C-   Controls: 
C-
C-   Created  18-NOV-1992   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER QUAD,PRIMAX,IAX,I,FIRST,IER
      REAL STEP,X,Y,Z,DX,DY,DZ
      REAL DT1,DT2,DSL(2),DIRO(3),VECT(6),VECTIN(6),VECTO(6)
      CHARACTER*4 HSHAPE
      INTEGER NSPAR,NBUF,IBUF
      REAL    SPAR(3),XPAR(3),ROTM(3,3),THICK,DL,MOM
C
      DATA FIRST/0/
      IF(FIRST.EQ.0) THEN
        STEP=5.
        FIRST=1
      ENDIF
      IF (QUAD.EQ.1.OR.QUAD.EQ.3) THEN
        THICK=109./2.
        IAX=1
      ELSE IF (QUAD.EQ.2.OR.QUAD.EQ.4) THEN
        THICK=109./2.
        IAX=2
      ELSE IF (QUAD.GT.4) THEN
        IAX=3
        THICK=152./2.
        IF(ABS(X).LT.81.3.AND.ABS(Y).LT.89.9) THEN   ! IN SAMUS
          THICK=165.6/2.
        ENDIF
      END IF
      VECT(1)=X
      VECT(2)=Y
      VECT(3)=Z
      DIRO(1)=DX
      DIRO(2)=DY
      DIRO(3)=DZ
      CALL VSCALE(DIRO(1),-1.,VECT(4),3)
      CALL UCOPY2(VECT(1),VECTO(1),6)
      DL=THICK/ABS(DIRO(IAX))
      CALL VLINE(VECT,1.,DIRO(1),DL,VECT(1),3)
 200  CONTINUE
      CALL MUTRNT(QUAD,MOM,STEP,VECT)
      IF (ABS(VECT(IAX)-VECTO(IAX)).LT.THICK) GO TO 200
      IF (PRIMAX.EQ.1) THEN
        CALL UCOPY2(VECT(1),VECTIN(1),6)
        DT1=-VECTO(5)/VECTO(4)+VECTIN(5)/VECTIN(4)
        DT2=-VECTO(6)/VECTO(4)+VECTIN(6)/VECTIN(4)
      ELSE IF (PRIMAX.EQ.2) THEN
        CALL UCOPY2(VECT(1),VECTIN(1),6)
        DT1=-VECTO(4)/VECTO(5)+VECTIN(4)/VECTIN(5)
        DT2=-VECTO(6)/VECTO(5)+VECTIN(6)/VECTIN(5)
      ELSE IF (PRIMAX.EQ.3) THEN
        CALL UCOPY2(VECT(1),VECTIN(1),6)
        DT1=-VECTO(4)/VECTO(6)+VECTIN(4)/VECTIN(6)
        DT2=-VECTO(5)/VECTO(6)+VECTIN(5)/VECTIN(6)
      END IF
      DSL(1)=DT1*MOM
      DSL(2)=DT2*MOM
C
  999 RETURN
      END
