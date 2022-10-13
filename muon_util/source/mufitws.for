      SUBROUTINE MUFITWS(ITRAK)
C----------------------------------------------------------------------
C-
C    Purpose and Methods : Fit a WAMUS BC/SAMUS A track
C
C        x=x0+(z-z0)*xin' 
C        y=y0+(z-z0)*yin'
C
C    Minimize chisq=sum[f(i)-DIST(i)]**2*WT(i) where
C
C        f(i) =sum[AA(i,k)*PAR(k)+BB(i)],
C        PAR=[x0,y0,xin',yin']  (vector of parameters)
C        AA(i,1)=c(i), AA(i,2)=s(i), AA(i,3)=z(i)*c(i), AA(i,4)=z(i)*s(i),
C        BB(i)=-x(i)*c(i)-y(i)*s(i)
C        x(i),y(i),z(i) = coordinates of center of wire i
C        s(i),c(i)  = sin & cos of drift direction wrt x axis
C        DIST(i) drift distance
C-
C-   Inputs  : ITRAK        - track number
C
C-   Outputs : updated MUOT bank
C-   Controls:
C-
C-   Created  20-JUN-1991   Daria Zieminska
C-   Updated   3-FEB-1992   Daria Zieminska  use SAMUS hits plus a
C-   "pseudopoint" at the center of the magnet
C-   Updated  30-MAR-1992   Daria Zieminska  use vertex
C-   Updated  27-JUN-1992   Daria Zieminska  store left-right assignment
C-                          (needed for global fitting)
C-   Updated  07-FEB-1995   Igor Mandrichenko
C-                          New format of STTH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSACL.LINK'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
      INTEGER ICALL,IFAIL,INDEX(4),N_DIR
      INTEGER NSAM,ISAM,QUAD,LSAHH,GZSAHH,LSTTH
      INTEGER ITRAK,LMUOT,GZMUOT,NPTRAK,NTOT,ITOT,NA,NBC
      INTEGER PASS,IVER,NV
      integer ii, jj
      REAL XMAGC,YMAGC,ZMAGC,WTX,WTY,VERTEX(3),WTVERX,WTVERY
      INTEGER  IPAR1,IPAR2
      INCLUDE 'D0$INC:PI.DEF'
      REAL XWIRE(10),YWIRE(10),ZWIRE(10),SINE(10),COSINE(10),WT(10)
      REAL RESID(10),RESID2,LR(10)
      REAL PAR(4) ,MOM
      REAL AA(10,4),BB(10),DIST(10)
      REAL CHISQ,SUMAA,WTDIST,WTAA
      REAL*8 COV(4,4),SUM(4),PAR8(4)
      LOGICAL REFIT
      SAVE ICALL,WTX,WTY
      INTEGER CELADR, IST, ISC
      REAL    GEO(6),DRDST
C
      DATA WTX,WTY,WTVERX,WTVERY/1.0,1.0,1.0,1.0/
      DATA ICALL/0/
C------------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
C        CALL EZPICK('MURECO_RCP')
C        CALL EZGET('CHIMAX',CHIMAX,IER)
C        CALL EZRSET
        ICALL=1
      END IF
      LMUOT=GZMUOT(ITRAK)
      NPTRAK=IQ(LMUOT+1)
      NSAM=IQ(LMUOT+2)
      NBC=NSAM/100
      NA=NSAM-NBC*100
      NSAM=NA
      QUAD=IQ(LMUOT+3)
      XMAGC=Q(LMUOT+11)
      YMAGC=Q(LMUOT+12)
      ZMAGC=Q(LMUOT+13)
      MOM=ABS(Q(LMUOT+23))
      WTVERX=MOM**2/1000000.
      WTVERX=MIN(WTVERX,1.)
      WTVERY=WTVERX
C      IF (QUAD.EQ.5.OR.QUAD.EQ.7.OR.QUAD.EQ.9.OR.QUAD.EQ.11) THEN
C        WTX=1.0
C        WTY=0.01
C      ELSE
C        WTX=0.01
C        WTY=1.
C      END IF
      IF (QUAD.GT.4.AND.QUAD.LT.9) THEN
        N_DIR=1
      ELSE
        N_DIR=2
      END IF
      CALL VERXYZ(IVER,VERTEX,NV)
      PASS=1
 2000 CONTINUE
      CHISQ=0.
      do ii=1, 4
        par8(ii) = 0
        sum(ii) = 0
        do jj=1, 4
          cov(ii,jj) = 0
        enddo
      enddo
      CALL VZERO(AA,40)
C
C  Get SAMUS points
C
      LSAHH   = GZSAHH()
      LSTTH=LQ(LMUOT-IZSTTH)
      ITOT=0
C
C  Use vertex
C
      ITOT=ITOT+1
      XWIRE(ITOT)=VERTEX(1)
      YWIRE(ITOT)=VERTEX(2)
      ZWIRE(ITOT)=VERTEX(3)-ZMAGC
      WT(ITOT)=WTVERX
      DIST(ITOT)=0.
      SINE(ITOT)=0.
      COSINE(ITOT)=1.
      ITOT=ITOT+1
      SINE(ITOT)=1.
      COSINE(ITOT)=0.
      DIST(ITOT)=0.
      XWIRE(ITOT)=VERTEX(1)
      YWIRE(ITOT)=VERTEX(2)
      ZWIRE(ITOT)=VERTEX(3)-ZMAGC
      WT(ITOT)=WTVERY
      DO 100 ISAM=1,NSAM
        CALL    GTSTTH(0, LSTTH, ISAM, IST, ISC, CELADR, GEO, DRDST)
C        I=IQ(LSTTH+2*ISAM-1)
C        LSAV     = LQ(LSAHH-I)
C        IHIT(ISAM)  = IQ(LSTTH+2*ISAM)
C        LHIT   = LQ(LSAV-IZSACL)+15*(IHIT(ISAM)-1)
C        LHIT   = LSAV+15*(IHIT(ISAM)-1)
        ITOT=ITOT+1
C        XWIRE(ITOT)=Q(LHIT+6)
C        YWIRE(ITOT)=Q(LHIT+7)
C        ZWIRE(ITOT)=Q(LHIT+8)
C        ZWIRE(ITOT)=ZWIRE(ITOT)-ZMAGC
        XWIRE(ITOT)=GEO(1)
        YWIRE(ITOT)=GEO(2)
        ZWIRE(ITOT)=GEO(3) - ZMAGC
C        WT(ITOT)=1./(Q(LHIT+15))**2
        WT(ITOT)=1.  ! temporary
        IF (PASS.EQ.1) THEN
          LR(ITOT)=1.
        END IF
        DIST(ITOT)=DRDST*LR(ITOT)
C        SINE(ITOT)=ABS(Q(LHIT+9))
C        COSINE(ITOT)=ABS(Q(LHIT+10))
        SINE(ITOT)=ABS(GEO(4))
        COSINE(ITOT)=ABS(GEO(5))
        IF (N_DIR.EQ.2.AND.SINE(ITOT).GT.0.7.AND.COSINE(ITOT).GT.0.7)
     &    THEN
          SINE(ITOT)=-SINE(ITOT)
        END IF
  100 CONTINUE
      IF (PASS.GT.1) GO TO 3000
C
C  Get WAMUS pseudopoint at the center of magnet; allow for different
C  errors in x and y
C
      ITOT=ITOT+1
      SINE(ITOT)=0.
      COSINE(ITOT)=1.
      XWIRE(ITOT)=XMAGC
      YWIRE(ITOT)=YMAGC
      ZWIRE(ITOT)=0.
      WT(ITOT)=WTX
      DIST(ITOT)=0.
      ITOT=ITOT+1
      SINE(ITOT)=1.
      COSINE(ITOT)=0.
      XWIRE(ITOT)=XMAGC
      YWIRE(ITOT)=YMAGC
      ZWIRE(ITOT)=0.
      WT(ITOT)=WTY
      DIST(ITOT)=0.
      NTOT=ITOT
 3000 CONTINUE
      DO 300 ITOT=1,NTOT
        AA(ITOT,1)=COSINE(ITOT)
        AA(ITOT,2)=SINE(ITOT)
        AA(ITOT,3)=ZWIRE(ITOT)*COSINE(ITOT)
        AA(ITOT,4)=ZWIRE(ITOT)*SINE(ITOT)
        BB(ITOT)  =-XWIRE(ITOT)*COSINE(ITOT)-YWIRE(ITOT)*SINE(ITOT)
        IF (PASS.EQ.1) THEN
          WTDIST=-BB(ITOT)*WT(ITOT) ! don't use SAMUS drift in 1st pass
        ELSE
          WTDIST=(DIST(ITOT)-BB(ITOT))*WT(ITOT)
        END IF
        DO 400 IPAR1=1,4
          SUM(IPAR1)=SUM(IPAR1)+ AA(ITOT,IPAR1)*WTDIST
  400   CONTINUE
        DO 500 IPAR1=1,4        ! calculate inverse of covariance matrix
          WTAA=AA(ITOT,IPAR1)*WT(ITOT)
          DO 501 IPAR2=IPAR1,4
            COV(IPAR1,IPAR2)=COV(IPAR1,IPAR2) + AA(ITOT,IPAR2)*WTAA
            IF (IPAR2.GT.IPAR1) COV(IPAR2,IPAR1)=COV(IPAR1,IPAR2)
  501     CONTINUE
  500   CONTINUE
  300 CONTINUE
C     invert covariance matrix:
      CALL DINV(4,COV,4,INDEX,IFAIL)   ! cernlib routine
      DO 600 IPAR1=1,4
        DO 900 IPAR2=1,4
          PAR8(IPAR1)=PAR8(IPAR1)+COV(IPAR1,IPAR2)*SUM(IPAR2)
  900   CONTINUE
        PAR(IPAR1)=PAR8(IPAR1)
  600 CONTINUE
C
C  Calculate fit residuals.
C
      REFIT=.FALSE.
      DO 700 ITOT=1,NTOT
        SUMAA = 0.0
        DO 800 IPAR1=1,4
          SUMAA=SUMAA + AA(ITOT,IPAR1)*PAR(IPAR1)
  800   CONTINUE
        RESID(ITOT)=DIST(ITOT)-BB(ITOT)-SUMAA
        IF (PASS.LT.10.AND.ITOT.GT.2.AND.ITOT.LE.2+NSAM) THEN
          RESID2=-DIST(ITOT)-BB(ITOT)-SUMAA
          IF (ABS(RESID(ITOT)).GT.ABS(RESID2)) THEN
            LR(ITOT)=-LR(ITOT)
            REFIT=.TRUE.
          END IF
        END IF
        CHISQ=CHISQ+RESID(ITOT)**2*WT(ITOT)
  700 CONTINUE
      IF (PASS.EQ.1.OR.REFIT) THEN
        PASS=PASS+1
        GO TO 2000
      END IF
      CHISQ=CHISQ/(FLOAT(NTOT)-4.)
      CALL MUOTWS(LMUOT,PAR,CHISQ)
C
C  Store lef-right assignments in SAMH
C      
C      DO 200 ISAM=1,NSAM
C        ITOT=2+ISAM
C        I=IQ(LSTTH+2*ISAM-1)
C        LSAV     = LQ(LSAHH-I)
C        IHIT(ISAM)  = IQ(LSTTH+2*ISAM)
C        LHIT   = LSAV+15*(IHIT(ISAM)-1)
C        Q(LHIT+13)=LR(ITOT)
C  200 CONTINUE
  999 CONTINUE
      RETURN
      END
