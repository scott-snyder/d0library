      FUNCTION SATRGLO(P,I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For momentum P integrate track through 
C-   magnetic field; return bending angle. Used in momentum fitting.
C-   Track lines before and after toroid are taken from MUON bank.
C-   This function applies to forward muons
C-
C-   Inputs  : 
C-              P     -      particle momentum 
C-              I     -      internal flag ( see CERN Long Up FOR MINVAR )
C-   Outputs : 
C-   Controls: 
C-
C-   Created  8-JUN-1992   O.Eroshin, Daria Zieminska  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     SATRGLO, SATRGLO_SET,SATRGLO_SAVE,MOM(4)
      REAL     VDIST,DXY,DSLO,DX,DY,DELX,DELY,WTX,WTY 
      INTEGER  I,II,LMUON,LMUOT,NSTEP,LABEL,IVER,NV,PLANE,QUAD
      REAL     P,X,Y,DIF,DIR,STEP,ZMAG,PT(3),VT(3),VECT(6)
      REAL     VIN(6),VOUT(3)
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      DATA     NSTEP/50/,ZMAG/82.8/
C----------------------------------------------------------------------
C
      ASSIGN 3 TO LABEL
C
    1 CONTINUE
      STEP   = 5.
      QUAD=IQ(LMUOT+3)
      IF (Q(LMUOT+13).LT.0.0)                   THEN
        DIF  = -ZMAG/Q(LMUOT+19)
        DIR  = Q(LMUOT+19)/ABS(Q(LMUOT+19))
                                                ELSE
        DIF  =  ZMAG/Q(LMUOT+19)
        DIR  = -Q(LMUOT+19)/ABS(Q(LMUOT+19))
        CALL VSCALE(Q(LMUOT+17),-1.,VECT(4),3)
      END IF
      CALL VSCALE(Q(LMUOT+17),DIR,VECT(4),3)
      CALL VSCALE(Q(LMUOT+17),DIR,VOUT,3)
      CALL VLINE(Q(LMUOT+11),1.,Q(LMUOT+17),DIF,VECT(1),3)
C
      CALL UCOPY(Q(LMUON+11),MOM,4)
      CALL UCOPY(Q(LMUON+37),VIN,3)
      VIN(4)=MOM(1)/MOM(4)
      VIN(5)=MOM(2)/MOM(4)
      VIN(6)=MOM(3)/MOM(4)
      CALL VSCALE(VIN(4),-1.,VIN(4),3)
      DX=ABS(VIN(1))
      DY=ABS(VIN(2))
      IF (DX.GT.DY) THEN
        PLANE=1
      ELSE
        PLANE=2
      END IF
C
    2 CONTINUE
      CALL SATRNT(P,STEP,VECT)
      IF (ABS(VECT(3)-Q(LMUOT+13)).LE.ZMAG)     GO TO 2
C
      GO TO LABEL
C
    3 CONTINUE
      DELX=VECT(4)/VECT(6)-VIN(4)/VIN(6)
      DELX=DELX*1000.
      DELY=VECT(5)/VECT(6)-VIN(5)/VIN(6)
      DELY=DELY*1000.
      IF (PLANE.EQ.1) THEN
        WTX=1. 
        WTY=0.
      ELSE
        WTX=0.
        WTY=1.
      END IF
C      IF (QUAD.EQ.5.OR.QUAD.EQ.7.OR.QUAD.EQ.9.OR.QUAD.EQ.11) THEN
C        WTX=1. 
C        WTY=0.01
C      ELSE
C        WTX=0.01
C        WTY=1.
C      END IF
      DXY=SQRT(WTX*DELX**2+WTY*DELY**2)
C     DXY=SQRT((DELX**2+DELY**2)/2.)
      SATRGLO=DXY 
      RETURN
C
C......   Save bank pointer on track
C
      ENTRY SATRGLO_SET(II)
      SATRGLO_SET=0.
      LMUON  = II
      LMUOT=LQ(LMUON-11)
      RETURN
C
C......   For momentum P calculate point and direction cosine
C         inside of magnet
C
      ENTRY SATRGLO_SAVE(P,PT,VT)
      SATRGLO_SAVE=DXY 
      ASSIGN 4 TO LABEL
      GO TO 1
C
    4 CONTINUE
      CALL UCOPY(VECT(1),PT,3)
      CALL UCOPY(VECT(4),VT,3)
      RETURN
      END
