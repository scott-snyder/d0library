      SUBROUTINE DRAWPT_XY(XPP,YPP,NPOINT,JFIX,LOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :         JFIX=1  free scale
C-                     JFIX=2  fixed scale
C-                     Xpp = array of abscissa
C-                     ypp = array of ordinates
C-                     Npoint= nb. of entry points
C-   Outputs :
C-
C-   Updated   2-MAY-1989   A. Zylberstejn  Arrays are printed if
C-                           JFIX is negative
C-   Updated  17-SEP-1991   A. Zylberstejn   routine POINT renamed
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPOINT
      REAL EX(3),X(132),Y(124),XPP(NPOINT),YPP(NPOINT)
      REAL AVERA,DX,DY,ORD,PAS,RNOR,SIGMA,YMAX,YMIN,V
      INTEGER I,IAUX(130),IDY,IFIX,IFOIS,II,IK,ISX,IWR,IX
      INTEGER J,JFIX,JJ,LK
      INTEGER K,KFIX,N,NK,NM,N1S,N1X,N2X,N3X,N4X
      INTEGER NY,NNY(2),NY1,XUN(132)
      CHARACTER *1 N1(131),N2(131),N3(131),N4(131)
      CHARACTER*131 A
      CHARACTER *1 IEB(10)
      INTEGER LOUT
      LOGICAL DO_X
      DATA IEB/'0','1','2','3','4','5','6','7','8','9'/
      DATA NNY/24,64/
      DATA IWR/6/
      DATA IFOIS/0/
C
C      IWR=LUNOUT
      IF(IFOIS.LE.0)THEN
        IWR=LOUT
        IF(LOUT.LE.0)IWR=6
        DO I=1,132
          XUN(I)=FLOAT(I)
        END DO
      END IF
      IFIX=MOD(IABS(JFIX),10)
C
 1000 FORMAT(1H1,5X,2A10,G12.4,' MOYENNE',G12.4,' SIG',G10.4,'HW',G10.4)
      IF(JFIX.LT.0)THEN
        WRITE(IWR,*)'IN DRAWPT_XY: XPP'
        WRITE(IWR,'(10F7.2)')(XPP(I),I=1,NPOINT)
        WRITE(IWR,*)' YPP'
        WRITE(IWR,'(10G10.4)')(YPP(I),I=1,NPOINT)
      END IF
      IF(NPOINT.GT.124)THEN
        ISX=NPOINT/124 +1
        K=0
        NM=NPOINT
        IF(MOD(NPOINT,2).NE.0)NM=NPOINT-1
        DO 10 I=1,NM,ISX
          K=K+1
          Y(K)=YPP(I)
          X(K)=XPP(I)
          DO 8 J=2,ISX
    8     Y(K)=Y(K)+Y(J+I-1)
   10   CONTINUE
        N=K
      ELSE
        DO 12 I=1,NPOINT
          Y(I)=YPP(I)
          X(I)=XPP(I)
          IAUX(I)=I
   12   CONTINUE
        N=NPOINT
      END IF
C  RECHERCHE DU MAX ET DU MIN
      EX(2)=-1000000.
      EX(1)=0.
      AVERA=0.
      SIGMA=0.
      PAS=1.
      NY=NNY(IFIX)
      EX(3)=0.
      CALL VEXCUM(Y,EX,N)
      YMAX=EX(2)
      YMIN=EX(1)
      IF(YMIN.LT.0.)YMIN=-1.
      WRITE(IWR,4432)YMAX,YMIN
      IF(YMAX.LE.0.)RETURN
 4432 FORMAT(' YMAX,YMIN',2G10.4)
      RNOR=1.
      IF(IFIX.EQ.1)THEN
        V=YMAX/10.
   14   V=V*10.
        IF(V.LT.100.)GO TO 14
        RNOR=V/YMAX
      END IF
C     ECART MAXIMUM
C      ECART=(YMAX-YMIN)/(YMIN+YMAX)
C     XMAX=X(N)
C     XMIN=X(1)
C  INTERVALLE EN X ET EN Y
C     DX=(XMAX-XMIN-1)/FLOAT(N)   +1
      DX=PAS
      DY=RNOR*(YMAX-YMIN)/NY
      IF(IFIX.EQ.2) DY=1.
      IF(DY.LE.1.)GO TO 101
      IDY=DY*10.
      IF(MOD(IDY,5).EQ.0)GO TO 101
      IDY=IDY/5
      DY=FLOAT(IDY+1)/2.
C      IDY=DY+1
C      DY=IDY
C      IF(DY.LT.1.)DY=1.
  101 WRITE(IWR,4568)RNOR,DY
 4568 FORMAT(' COEFICIENT DE NORMALISATION',G12.5,' PAS DE :',G10.4)
      IF(DY.EQ.0.) RETURN
C      A(N/2)='I'
      N1S=0
C      IF(IFOIS.NE.1)GO TO 16
      DO 113 I=1,N
C     IX=(XPP(I)-XMIN)/DX   +1.
        IX=I
        N1X=XPP(I)/1000.
        N1S=N1S+N1X
        N2X=(XPP(I)-N1X*1000.)/100.
        N3X=(XPP(I)-N1X*1000.-N2X*100.)/10.
        N4X=AMOD(XPP(I),10.)
        N1(IX)=IEB(N1X+1)
        IF(N1(IX).EQ.'0')N1(IX)=' '
        N2(IX)=IEB(N2X+1)
        IF(N1(IX).EQ.' ' .AND.N2(IX).EQ.'0')N2(IX)=' '
        N3(IX)=IEB(N3X+1)
        N4(IX)=IEB(N4X+1)
  113 CONTINUE
   16 CONTINUE
      NY1=NY+1
      A=' '
      NK=N
      DO 40 II=1,NY
        IF(NK.EQ.0)GO TO 32
        JJ=NY1-II
        ORD=RNOR*YMIN+(JJ-1)*DY
        IF(ORD.GT.YMAX*RNOR)GO TO 40
        LK=0
        DO 30 IK=1,NK
          I=IAUX(IK)
          IX=I
C       IF(A(IX:IX).EQ.'X')GO TO 30
C      IF(RNOR*Y(I).GE.ORD.AND.RNOR*Y(I).GE.0.)A(IX)='X'
          IF(RNOR*Y(I).GE.ORD)THEN
            A(IX:IX)='X'
          ELSE
            LK=LK+1
            IAUX(LK)=IAUX(IK)
          END IF
   30   CONTINUE
        NK=LK
   32   WRITE(IWR,5)ORD,(A(J:J),J=1,N)
   37   CONTINUE
   40 CONTINUE
      WRITE(IWR,2604)
 2604 FORMAT(' ')
      IF(N1S.NE.0)WRITE(IWR,7)(N1(I),I=1,N)
      WRITE(IWR,7)(N2(I),I=1,N)
      WRITE(IWR,7)(N3(I),I=1,N)
      WRITE(IWR,7)(N4(I),I=1,N)
    5 FORMAT(1X,F6.0,131A1)
    7 FORMAT(7X,131A1)
      RETURN
      END
