      SUBROUTINE MUFITA(ITRAK,ZREF,PAR,COV,IER)
C----------------------------------------------------------------------
C-
C    Purpose and Methods : Fit an A segment of a forward track using 
C    WAMUS  data
C
C       fx=xref+(z-zref)*xin' 
C       fy=yref+(z-zref)*yin' 
C
C    Minimize chisq=sum[f(i)-MEAS(i)]**2*WT(i) where
C
C        f(i) =sum[AA(i,k)*PAR(k)],
C        PAR=[xref,yref,xin',yin']  (vector of parameters)
C        AA(i,1)=c(i), AA(i,2)=s(i), AA(i,3)=z(i)*c(i), AA(i,4)=z(i)*s(i),
C        MEAS(i)=x(i) or y(i) [drift or wire measured coord]
C        x(i),y(i),z(i) = measured wamus hit coords
C        s(i),c(i)  = sin & cos of drift direction wrt x axis
C-
C-   Inputs  : ITRAK        - track number
C
C-   Outputs : updated MUOT bank   
C-   Controls: 
C-
C-   Created  1-SEP-1992   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSACL.LINK'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER ICALL,IFAIL,INDEX(4),IER,N_DIR,K
      INTEGER NWAM,NX,NY,IWAM,ISAM,QUAD,ORENT,LSAHH,GZSAHH,LSTTH
      INTEGER ITRAK,LMUOT,GZMUOT,NPTRAK,NT,NHT,IND,NTOT,ITOT,NA,NBC,I1
      INTEGER IWADD,IHMUOH,TIMSIN,LMUOH,GZMUOH,IDELT,IPAD 
      INTEGER PASS,IVER,NV,IMAX,I,II,IHIT(12) 
      INTEGER  LHIT,LSAV,GZMTRH,IPAR,IPAR1,IPAR2,NBAD
      integer jj
      REAL XWAM(40),YWAM(40),ZWAM(40),SWAM(40),CWAM(40),WTWAM(40) 
      REAL XWIRE(50),YWIRE(50),ZWIRE(50),SINE(50),COSINE(50),WT(50)
      REAL RESID(50),RESID2,LR(50),BAD(50)
      REAL PAR(4),COV(4,4),MOM,XREF,YREF,ZREF
      REAL XMAGC,YMAGC,ZMAGC,CONST,COSX,COSY,COSZ 
      REAL AA(50,4),BB(50),DIST(50),MEAS(50)
      REAL CHIMAX,CHISQ,CHISQ1,CHISQ1MAX,SUMAA,WTDIST,WTAA 
      REAL*8 COV8(4,4),DET,SUM(4),PAR8(4) 
      LOGICAL REFIT
      SAVE ICALL,CHIMAX
C
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
      QUAD=IQ(LMUOT+3)
      XMAGC=Q(LMUOT+11)
      YMAGC=Q(LMUOT+12)
      ZMAGC=Q(LMUOT+13)
      PASS=1
      NBAD=0
      CALL VZERO(BAD,50)
C
C  Get WAMUS points
C
      IER=0
      CALL MUHITA(ITRAK,NPTRAK,NWAM,NX,NY,
     1     XWAM,YWAM,ZWAM,SWAM,CWAM,WTWAM)
      IF (NX.LT.2.OR.NY.LT.2) THEN
        IER=1
        GO TO 999
      END IF
      CALL UCOPY2(XWAM,XWIRE,NWAM)
      CALL UCOPY2(YWAM,YWIRE,NWAM)
      CALL UCOPY2(ZWAM,ZWIRE,NWAM)
      CALL UCOPY2(SWAM,SINE,NWAM)
      CALL UCOPY2(CWAM,COSINE,NWAM)
      CALL UCOPY2(WTWAM,WT,NWAM)
      DO IWAM=1,NWAM
        ZWIRE(IWAM)=ZWIRE(IWAM)-ZREF
      END DO
 2000 CONTINUE
      CHISQ=0.
      do ii=1, 4
        par8(ii) = 0
        sum(ii) = 0
        do jj=1, 4
          cov8(ii,jj) = 0
        enddo
      enddo
      CALL VZERO(AA,40)
C
      NTOT=NWAM 
      DO 300 ITOT=1,NTOT
        IF (BAD(ITOT).GT.0.) WT(ITOT)=0.
        AA(ITOT,1)=COSINE(ITOT)
        AA(ITOT,2)=SINE(ITOT)
        AA(ITOT,3)=ZWIRE(ITOT)*COSINE(ITOT)
        AA(ITOT,4)=ZWIRE(ITOT)*SINE(ITOT)
        MEAS(ITOT)=COSINE(ITOT)*XWIRE(ITOT)+SINE(ITOT)*YWIRE(ITOT)
C
        DO 400 IPAR1=1,4
          SUM(IPAR1)=SUM(IPAR1)+ AA(ITOT,IPAR1)*MEAS(ITOT)*WT(ITOT)
  400   CONTINUE
C
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
        RESID(ITOT)=MEAS(ITOT)-SUMAA
 1100   CONTINUE  
        CHISQ1=RESID(ITOT)**2*WT(ITOT)
        CHISQ=CHISQ+CHISQ1
        IF (CHISQ1.GT.CHISQ1MAX) THEN 
          CHISQ1MAX=CHISQ1
          IMAX=ITOT
        END IF
  700 CONTINUE
      IF (NTOT.GT.4) THEN
        CHISQ=CHISQ/(FLOAT(NTOT)-4.)
      ELSE
        CHISQ=0.
      END IF
      XREF=PAR(1)
      YREF=PAR(2)
      CONST=SQRT(1.+PAR(3)**2+PAR(4)**2)
      IF (Q(LMUOT+13).LT.0.) THEN
        CONST=-CONST
      END IF
      COSX=PAR(3)/CONST
      COSY=PAR(4)/CONST
      COSZ=1./CONST
C      IF ( ZREF.EQ.ZMAGC) THEN
C        WRITE (0,102) XMAGC,Q(LMUOT+11),YMAGC,Q(LMUOT+12),
C     1       COSX,Q(LMUOT+17),COSY,Q(LMUOT+18),COSZ,Q(LMUOT+19)
C  102   FORMAT(' MUFITA   XMAGC YMAGC COSOUT',10F10.3)
C      ENDIF
  999 RETURN
      END
