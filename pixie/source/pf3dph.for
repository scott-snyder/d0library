      SUBROUTINE PF3DPH(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw one FDC Phi sector in 3-D
C-
C-   Inputs  : HALF,SECTOR = FDC Phi sector desired
C-   Outputs : Display on screen
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated  30-MAR-1992   Robert E. Avery  Change  color name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER HALF,SECTOR,LAYER,MODULE,I
      REAL DIMENS(6),DIR,XC,YC,ZC,X(4),Y(4),Z(4),DZ
      REAL INRAD,OUTRAD,BEGARC,ENDARC,ARC,DEGRAD
      CHARACTER*4 PSECLR
      CHARACTER*4 CVAL, REM
      INTEGER TYP,IVAL,IER
      LOGICAL EZERROR
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3DPH','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','FDC COLR SECTOR',1,
     &     IVAL,PSECLR,TYP,REM,IER)
      DEGRAD=PI/180.
      LAYER=2
      DIR=-1.
      IF(HALF.EQ.1) DIR=1.
      MODULE=HALF*3+LAYER
      CALL GTFWAL(MODULE,DIMENS)
      IF(DIMENS(1).LT. 0.0) GOTO 999
      XC=DIMENS(4)
      YC=DIMENS(5)
      ZC=DIMENS(6)
      INRAD =DIMENS(1)
      OUTRAD=DIMENS(2)
      DZ=DIMENS(3)
      BEGARC=SECTOR*10.
      ENDARC=(SECTOR+1)*10.
      CALL PXCOLR(PSECLR)
      CALL JPINTR(1)
      CALL JARC(XC,YC,(ZC-DZ),INRAD,0,BEGARC,ENDARC)
      CALL JARC(XC,YC,(ZC+DZ),INRAD,0,BEGARC,ENDARC)
      CALL JARC(XC,YC,(ZC-DZ),OUTRAD,0,BEGARC,ENDARC)
      CALL JARC(XC,YC,(ZC+DZ),OUTRAD,0,BEGARC,ENDARC)
      DO 5 ARC=BEGARC,ENDARC,(ENDARC-BEGARC)
        X(1)=INRAD*COSD(ARC)
        Y(1)=INRAD*SIND(ARC)
        Z(1)=ZC-DZ
        X(2)=INRAD*COSD(ARC)
        Y(2)=INRAD*SIND(ARC)
        Z(2)=ZC+DZ
        X(3)=OUTRAD*COSD(ARC)
        Y(3)=OUTRAD*SIND(ARC)
        Z(3)=ZC+DZ
        X(4)=OUTRAD*COSD(ARC)
        Y(4)=OUTRAD*SIND(ARC)
        Z(4)=ZC-DZ
        CALL J3MOVE(X(4),Y(4),Z(4))
        DO 10 I=1,4
          CALL J3DRAW(X(I),Y(I),Z(I))
   10   CONTINUE
    5 CONTINUE
C----------------------------------------------------------------------
  990 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
