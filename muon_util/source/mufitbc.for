      SUBROUTINE MUFITBC(ITRAK)
C----------------------------------------------------------------------
C-
C    Purpose and Methods : Fit a BC segment of a forward track using 
C    WAMUS and SAMUS data
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
C-   Created  1-SEP-1992   Daria Zieminska
C-   Updated  3-NOV-1994   Qizhong Li-Demarteau  fixed memory overwritting by
C-                                               increasing IHIT length 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSACL.LINK'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER ICALL,IFAIL,INDEX(4),IER,N_DIR,K
      INTEGER NSAM,NSAMUSED,NWAM,IWAM,ISAM,QUAD,ORENT,LSAHH,GZSAHH,LSTTH
      INTEGER ITRAK,LMUOT,GZMUOT,NPTRAK,NT,NHT,IND,NTOT,ITOT,NA,NBC,I1
      INTEGER IWADD,IHMUOH,TIMSIN,LMUOH,GZMUOH,IDELT,IPAD,NGOOD
      INTEGER PASS,IVER,NV,IMAX,I,II,IHIT(50) 
      INTEGER  LHIT,LSAV,GZMTRH,IPAR,IPAR1,IPAR2,NBAD
      REAL XWAM(40),YWAM(40),ZWAM(40),SWAM(40),CWAM(40),WTWAM(40),WTSAM
      REAL XWIRE(50),YWIRE(50),ZWIRE(50),SINE(50),COSINE(50),WT(50)
      REAL RESID(50),RESID2,LR(50),BAD(50)
      REAL PAR(4),COV(4,4),MOM
      REAL XMAGC,YMAGC,ZMAGC,CONST,COSX,COSY,COSZ 
      REAL AA(50,4),BB(50),DIST(50)
      REAL CHIMAX,CHISQ,CHISQ1,CHISQ1MAX,SUMAA,WTDIST,WTAA 
      REAL*8 COV8(4,4),DET,SUM(4),PAR8(4) 
      LOGICAL REFIT
      SAVE ICALL,CHIMAX
C
      DATA ICALL/0/
      DATA WTSAM/1000./  ! assume sigma about 300 microns
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
      NSAM=NA+NBC
      IF (NBC.LT.1) GO TO 999   ! Don't refit if no SAMUS BC hits 
      QUAD=IQ(LMUOT+3)
      XMAGC=Q(LMUOT+11)
      YMAGC=Q(LMUOT+12)
      ZMAGC=Q(LMUOT+13)
      IF (QUAD.GT.4.AND.QUAD.LT.9) THEN
        N_DIR=1
      ELSE
        N_DIR=2
      END IF
      PASS=1
      ITOT=0
      NBAD=0
      CALL VZERO(BAD,50)
C
C  Get WAMUS points
C
      CALL MUHITBC(ITRAK,NPTRAK,NWAM,XWAM,YWAM,ZWAM,SWAM,CWAM,WTWAM)
      CALL UCOPY2(XWAM,XWIRE,NWAM)
      CALL UCOPY2(YWAM,YWIRE,NWAM)
      CALL UCOPY2(ZWAM,ZWIRE,NWAM)
      CALL UCOPY2(SWAM,SINE,NWAM)
      CALL UCOPY2(CWAM,COSINE,NWAM)
      CALL UCOPY2(WTWAM,WT,NWAM)
      DO IWAM=1,NWAM
        ZWIRE(IWAM)=ZWIRE(IWAM)-ZMAGC
      END DO
 2000 CONTINUE
      ITOT=NWAM
      CHISQ=0.
      CALL VZERO(PAR8,8)
      CALL VZERO(SUM,8)
      CALL VZERO(COV8,32)
      CALL VZERO(AA,40)
      CALL VZERO(DIST,NWAM)
C
C  Get SAMUS points
C
      LSAHH   = GZSAHH()
      LSTTH=LQ(LMUOT-IZSTTH)
      NSAMUSED=0
      DO 100 ISAM=1,NSAM
        I=IQ(LSTTH+2*ISAM-1)
        LSAV     = LQ(LSAHH-I)
        IHIT(ISAM)  = IQ(LSTTH+2*ISAM)
C        LHIT   = LQ(LSAV-IZSACL)+15*(IHIT(ISAM)-1)
        LHIT   = LSAV+15*(IHIT(ISAM)-1)
        IF (ABS(Q(LHIT+8)).LT.500.) GO TO 100  ! Skip A station hits
C        IF (ABS(Q(LHIT+8)).GT.800.) GO TO 100  ! Temp. skip C station
        NSAMUSED=NSAMUSED+1
        I1=ISAM
        ITOT=ITOT+1
        XWIRE(ITOT)=Q(LHIT+6) 
        YWIRE(ITOT)=Q(LHIT+7) 
        ZWIRE(ITOT)=Q(LHIT+8)-ZMAGC 
C        WT(ITOT)=1./(Q(LHIT+15))**2
        IF (PASS.EQ.1) THEN
          LR(ITOT)=1.
          WT(ITOT)=1. 
        ELSE 
          WT(ITOT)=WTSAM  
        END IF
        DIST(ITOT)=Q(LHIT+14)*LR(ITOT)
        SINE(ITOT)=ABS(Q(LHIT+9))
        COSINE(ITOT)=ABS(Q(LHIT+10)) 
        IF (N_DIR.EQ.2.AND.SINE(ITOT).GT.0.7.AND.COSINE(ITOT).GT.0.7)
     &    THEN
          IF (ABS(Q(LHIT+8)).GT.800.) THEN
            SINE(ITOT)=-SINE(ITOT)
          END IF
        END IF
        IF (N_DIR.EQ.1.AND.SINE(ITOT).GT.0.7.AND.COSINE(ITOT).GT.0.7)
     &    THEN
          IF (ABS(Q(LHIT+8)).LT.800.) THEN
            SINE(ITOT)=-SINE(ITOT)
          END IF
        END IF
  100 CONTINUE
      NTOT=ITOT
      IF (NTOT.LT.5) GO TO 999
      DO 300 ITOT=1,NTOT
        IF (BAD(ITOT).GT.0.) WT(ITOT)=0.
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
            COV8(IPAR1,IPAR2)=COV8(IPAR1,IPAR2) + AA(ITOT,IPAR2)*WTAA
            IF (IPAR2.GT.IPAR1) COV8(IPAR2,IPAR1)=COV8(IPAR1,IPAR2)
  501     CONTINUE
  500   CONTINUE
  300 CONTINUE
C     invert covariance matrix:
      CALL DINV(4,COV8,4,INDEX,IFAIL)   ! cernlib routine
      DO 600 IPAR1=1,4
        DO 900 IPAR2=1,4
          PAR8(IPAR1)=PAR8(IPAR1)+COV8(IPAR1,IPAR2)*SUM(IPAR2)
          COV(IPAR1,IPAR2)=COV8(IPAR1,IPAR2)
  900   CONTINUE
        PAR(IPAR1)=PAR8(IPAR1)
  600 CONTINUE
C
C  Calculate fit residuals.
C
      NGOOD=NTOT-NBAD
      REFIT=.FALSE.
      CHISQ1MAX=0.
      DO 700 ITOT=1,NTOT
        SUMAA = 0.0
        DO 800 IPAR1=1,4
          SUMAA=SUMAA + AA(ITOT,IPAR1)*PAR(IPAR1)
  800   CONTINUE
        IF (WT(ITOT).EQ.0.) THEN
          RESID(ITOT)=0.
          GO TO 1100
        END IF
        RESID(ITOT)=DIST(ITOT)-BB(ITOT)-SUMAA
        IF (PASS.LT.5.AND.ITOT.GT.NWAM) THEN ! SAMUS hit; solve L-R 
          RESID2=-DIST(ITOT)-BB(ITOT)-SUMAA
          IF (ABS(RESID(ITOT)).GT.ABS(RESID2)) THEN
            LR(ITOT)=-LR(ITOT)
            REFIT=.TRUE.
          END IF
        END IF
 1100   CONTINUE  
        CHISQ1=RESID(ITOT)**2*WT(ITOT)
        CHISQ=CHISQ+CHISQ1
        IF (CHISQ1.GT.CHISQ1MAX) THEN 
          CHISQ1MAX=CHISQ1
          IMAX=ITOT
        END IF
  700 CONTINUE
      CHISQ=CHISQ/(FLOAT(NGOOD)-4.)
      IF (PASS.LT.5) THEN
        IF (PASS.EQ.1.OR.REFIT) THEN
          PASS=PASS+1
          GO TO 2000
        END IF
      END IF
C
C  Remove bad hits and refit if necessary
C
        IF (CHISQ1MAX.GT.5.) THEN
          BAD(IMAX)=1.
          NBAD=NBAD+1
          NGOOD=NTOT-NBAD
          IF (NGOOD.LT.5) THEN
            GO TO 999            ! should update flags
          END IF
          IF (NBAD.LT.NSAMUSED) THEN
            PASS=2
            GO TO 2000
          END IF
        END IF
C
C  Store lef-right assignments in SAMH
C      
      DO 200 ISAM=I1,NSAM  ! start with the first hit used in this fit
        ITOT=NWAM+ISAM
        I=IQ(LSTTH+2*ISAM-1)
        LSAV     = LQ(LSAHH-I)
        IHIT(ISAM)  = IQ(LSTTH+2*ISAM)
        LHIT   = LSAV+15*(IHIT(ISAM)-1)
        Q(LHIT+13)=LR(ITOT)
  200 CONTINUE
      XMAGC=PAR(1)
      YMAGC=PAR(2)
      CONST=SQRT(1.+PAR(3)**2+PAR(4)**2)
      IF (Q(LMUOT+13).LT.0) THEN
        CONST=-CONST
      END IF
      COSX=PAR(3)/CONST
      COSY=PAR(4)/CONST
      COSZ=1./CONST
C
C  Update MUOT bank; skip if bad fit 
C      
      IF (CHISQ.GT.5.) THEN
        GO TO 999
      END IF
      Q(LMUOT+11)=XMAGC
      Q(LMUOT+12)=YMAGC
      Q(LMUOT+17)=COSX 
      Q(LMUOT+18)=COSY 
      Q(LMUOT+19)=COSZ 
  999 RETURN
      END
