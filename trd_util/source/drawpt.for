      SUBROUTINE DRAWPT(YPP,NPOINT,IFIX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DRAW A "HBOOK TYPE HISTOGRAMM
C-
C-   Inputs  : YPP    =ARRAY OF BIN CONTENTS
C-             NPOINT =NB. OF BINS
C-             IFIX   =1   FREE SCALE
C-                    =2   FIXED "
C-   Outputs :
C-
C-   Created   A LONG TIME AGO AT SACLAY
C-   Updated  24-APR-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NPOINT
      REAL DX,DY,ORD,PAS,REM,RNOR,YMAX,YMIN,V
      REAL Y(124),YPP(NPOINT)
      INTEGER I,IDY,IFIX,IFOIS,II,IJK,IWR,IX,J,JJ,K
      INTEGER LPX0,LPX,N,NM,NNY(2),NY,X(132)
      INTEGER N1S,N1X,N2X,N3X,N4X,NY1,LOUT,TRUNIT
      CHARACTER *125 N1,N2,N3,N4,A
      CHARACTER *1 IEB(10)
      DATA IEB/'0','1','2','3','4','5','6','7','8','9'/
      DATA NNY/50,64/
      DATA IWR/26/
      DATA IFOIS,LPX0/2*0/
      LOUT=TRUNIT()
C
C
      IF(LOUT.LE.0)GO TO 999
      IF(NPOINT.LE.2)THEN
        CALL ERRMSG(' PROBLEM_TRD: TOO FEW DATA POINTS ',
     &    'DRAWPT',' ','W')
        GO TO 999
      END IF
      IF(NPOINT.GT.124)THEN
        IF(NPOINT.GT.248)THEN
          CALL ERRMSG(' PROBLEM_TRD: too many data points ',
     &      'DRAWPT',' ','W')
          GO TO 999
        END IF
        K=0
        NM=NPOINT
        IF(MOD(NPOINT,2).NE.0)NM=NPOINT-1
        DO 10 I=1,NM,2
          K=K+1
          Y(K)=(YPP(I)+YPP(I+1))*.5
          X(K)=I
   10   CONTINUE
        LPX=2
        IF(MOD(NPOINT,2).NE.0)THEN ! THE NB. OF BINS IS ODD
          K=K+1
          Y(K)=YPP(NPOINT)
          X(K)=2*X(K-1)-X(K-2)
        END IF
        N=K
      ELSE
        DO 12 I=1,NPOINT
          Y(I)=YPP(I)
          X(I)=I
   12   CONTINUE
        N=NPOINT
        LPX=1
      END IF
C  LOOK FOR MAX AND MIN
      YMAX=-100000.
      YMIN=0.
      PAS=1.
      NY=NNY(IFIX)
 4554 FORMAT(10F10.2)
      DO 13 I=1,N
        YMAX=AMAX1(Y(I),YMAX)
C      IF(IFOIS.LE.2)WRITE(LOUT,*)I,Y(I),YMAX
   13 YMIN=AMIN1(Y(I),YMIN)
      IF(IFIX.EQ.2)YMIN=0.
      IF(YMAX.LE.0.)GO TO 999
      RNOR=1.
      IF(YMAX.LT.10. AND. IFIX.EQ.1)THEN
        V=YMAX/10.
   14   V=V*10.
        IF(V.LT.100.)GO TO 14
        RNOR=V/YMAX
      END IF
C  X AND Y BINNING
C     DX=(XMAX-XMIN-1)/FLOAT(N)   +1
      DX=PAS
      DY=RNOR*(YMAX-YMIN)/NY
      IF(IFIX.EQ.2) DY=1.
      IF(DY.GT.1.)THEN
        REM=AMOD(DY,.5)
        IF(REM.EQ.0.)GO TO 101
        DY=DY-REM+.5
      ELSE IF(DY.LT. 0.5)THEN
        DY=.5
      ELSE IF(DY.GT.0.5)THEN
        DY=1.
      END IF
  101 CONTINUE
      WRITE(LOUT,4568)RNOR,DY,YMAX,YMIN
 4568 FORMAT(' NORMALIZATION ',G12.5,' VERTICAL STEP :',G10.4,
     +'   YMAX :',G10.4,' YMIN:',G10.4)
      IF(DY.EQ.0.) GO TO 999
      N1S=0
      IF(LPX.EQ.LPX0)GO TO 16
      LPX0=LPX
      N1=' '
      N2=' '
      N3=' '
      N4=' '
      DO 113 I=1,N
C     IX=(X(I)-XMIN)/DX   +1.
        IX=I
        N1X=X(I)/1000.
        N1S=N1S+N1X
        N2X=MOD(X(I)/100,10)
        N3X=MOD(X(I)/10,10)
        N4X=MOD(X(I),10)
        IF(N1X.NE.0)N1(IX:IX)=IEB(N1X+1)
        IF(N1X+N2X.NE.0)N2(IX:IX)=IEB(N2X+1)
        IF(N1X+N2X+N3X.NE.0)N3(IX:IX)=IEB(N3X+1)
        N4(IX:IX)=IEB(N4X+1)
  113 CONTINUE
   16 CONTINUE
      NY1=NY+1
      A=' '
      DO 40 II=1,NY
        JJ=NY1-II
        ORD=RNOR*YMIN+(JJ-1)*DY
        IF(ORD.GT.YMAX*RNOR)GO TO 40
        DO30 I=1,N
        IX=I
        IF(RNOR*Y(I).GE.ORD)A(IX:IX)='X'
   30   CONTINUE
        WRITE(LOUT,5)ORD,(A(I:I),I=1,NPOINT)
   40 CONTINUE
      WRITE(LOUT,2604)
 2604 FORMAT(' ')
      IF(N1.NE.' ')WRITE(LOUT,7)N1
      IF(N1.NE.' ' .OR. N2.NE.' ')WRITE(LOUT,7)N2
      WRITE(LOUT,7)N3
      WRITE(LOUT,7)(N4)
      WRITE(LOUT,2604)
    5 FORMAT(1X,F6.0,125A1)
    7 FORMAT(7X,A125)
  999 RETURN
      END
