      SUBROUTINE MCGLOBAL(LMUON,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Global fitting of a central muon track.
C-
C-   Inputs  :  LMUON  bank address
C-   Outputs :  IERR   =0 OK, =1 no ZTRK, =2 no VTXT, =3 NO DTRK)
C-                    =4 bad WAMUS A layer
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT
C-
C-   Created   5-SEP-1992   Daria Zieminska
C-   Updated  14-MAY-1993   Daria Zieminska  use Calorimeter information
C-   Updated  18-APR-1995   Daria Zieminska  status 110 independent of chisq 
C-   Updated  27-JUN-1995   Daria Zieminska  use GFITA 
C-   Updated  08-Aug-1995   Darien Wood      skip A stubs when GFITA=0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LMUON,IMUON,LMUOT,LVERT,LZTRK,LDTRK,IERR 
      INTEGER NVERT,GZMUON
      INTEGER QUAD,GFIT,GFITA,IER 
      REAL CLIST(75),P,P1,DP,COSIN(3),COSOUT(3),DIF(3)
      REAL PHI,THETA,ETA,PT,ELCAL,ELFE,ELOSS
      REAL RADLEN,DP2,DRHO2,DPOP,MCS,CHICUT 
      REAL DYDX,DZDX,DXDY,DZDY,ECUT
      LOGICAL FIRST
      DATA CHICUT,FIRST/10.,.TRUE./
C
      IF (FIRST) THEN
        CALL EZPICK('MURECO_RCP')
        FIRST=.FALSE.
        CALL EZGET('CHI_MIN_CD',CHICUT,IER)
        CALL EZGET_i('GFIT',GFIT,IER)
        CALL EZGET_i('GFITA',GFITA,IER)
        ECUT=20.
        CALL EZRSET
      END IF
      IERR=0
      LMUOT=LQ(LMUON-11)
      QUAD=IQ(LMUOT+3)
      IF (QUAD.GT.4) GO TO 999
      LVERT=LQ(LMUON-12)
      NVERT=-1
      IF (LVERT.GT.0) THEN
        NVERT=IQ(LVERT-5)
      ELSE
        GO TO 999
      END IF
      IMUON=IQ(LMUON-5)
      LZTRK=LQ(LMUON-13)
      IF(IQ(LMUOT+4).EQ.5) THEN ! A layer stub
        IF (GFITA.GT.0) THEN
        IF (LZTRK.EQ.0) THEN
          IERR=1
          GO TO 999
        END IF
        IF (LZTRK.GT.0) THEN
          LDTRK=LQ(LZTRK-7)
          IF (LDTRK.EQ.0) THEN
            IERR=3
            GO TO 999
          END IF
        END IF
        CALL MULOFIT(IMUON)    ! make MSEG banks for this track
        LMUON=GZMUON(IMUON)
        CALL MUFITASTUB(LMUON,CLIST,IERR)
        IF (CLIST(1).LT.1.OR.IERR.GT.4) THEN
          IQ(LMUON+4)=2
          GO TO 999
        END IF
C
C  Store chisq/df in MUON bank; if bad chisq don't update momentum in MUON bank
C
        Q(LMUON+31)=CLIST(3)/CLIST(2)  ! chisq/df
        IF (Q(LMUON+31).GT.CHICUT) THEN
          IQ(LMUON+4)=2
          GO TO 999
        END IF
        IF (MOD(QUAD,2).EQ.1) THEN
          DYDX=CLIST(5)
          DZDX=CLIST(7)
          COSIN(1)=1./SQRT(1.+DYDX**2+DZDX**2)
          IF (QUAD.EQ.3.OR.QUAD.EQ.7.OR.QUAD.EQ.11) THEN
            COSIN(1)=-COSIN(1)
          END IF
          COSIN(2)=DYDX*COSIN(1)
          COSIN(3)=DZDX*COSIN(1)
        ELSE IF (MOD(QUAD,2).EQ.0) THEN
          DXDY=CLIST(5)
          DZDY=CLIST(7)
          COSIN(2)=1./SQRT(1.+DXDY**2+DZDY**2)
          IF (QUAD.EQ.4.OR.QUAD.EQ.8.OR.QUAD.EQ.12) THEN
            COSIN(2)=-COSIN(2)
          END IF
          COSIN(1)=DXDY*COSIN(2)
          COSIN(3)=DZDY*COSIN(2)
        END IF
        PHI = ATAN2(COSIN(2),COSIN(1))
        IF (COSIN(2).LT.0.) PHI = PHI + TWOPI
        THETA = ACOS(COSIN(3))
C        write(0,101) quad,q(lmuon+14),phi,theta,clist(3) 
C  101   format(' Astub QUAD chisq',i5,4f12.2)
        IF (ABS(COSIN(3)).LT.0.999) ETA=-ALOG(TAN(THETA/2.))
        IQ(LMUON+4)=111   ! vertex, CDC AND CAL used in the fit
        IQ(LMUON+10)=8
        Q(LMUON+16)=THETA
        Q(LMUON+17)=ETA
        Q(LMUON+18)=PHI
        ENDIF
        GO TO 999   
      END IF  ! end ASTUB 
      CALL MULOFIT(IMUON)    ! make MSEG banks for this track
      LMUON=GZMUON(IMUON)
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.GT.0) THEN
        LDTRK=LQ(LZTRK-7)
        IF (LDTRK.GT.0) THEN
          CALL MUFITDABC(LMUON,CLIST,IERR)  ! use CDC and MUOT 
          IQ(LMUON+4)=2 
          IF (CLIST(1).GT.1..AND.IERR.LE.4) THEN
            Q(LMUON+31)=CLIST(3)/CLIST(2)  ! chisq/df 
            IF (Q(LMUON+31).LT.CHICUT) THEN
              IQ(LMUON+4)=11   ! vertex and CD used 
            END IF
          END IF
        END IF
      END IF
      IF (IQ(LMUON+4).EQ.0.OR.IQ(LMUON+4).EQ.2) THEN      
        IF (QUAD.LT.5) THEN
          CALL MUFITCFCAL(LMUON,CLIST,IERR)  ! no CDC; use MTC 
          IQ(LMUON+4)=2 
          IF (CLIST(1).GT.1..AND.IERR.LE.4) THEN
            Q(LMUON+31)=CLIST(3)/CLIST(2)  ! chisq/df 
c            IF (Q(LMUON+31).LT.CHICUT) THEN  
              IQ(LMUON+4)=110   ! vertex and CAL used 
c            END IF
          END IF
        END IF
      END IF
      IF (IQ(LMUON+4).NE.11.AND.IQ(LMUON+4).NE.110) THEN
        GO TO 999
      END IF
C
C  update MUON bank 
C
      IF (MOD(QUAD,2).EQ.1) THEN
        DYDX=CLIST(5)
        DZDX=CLIST(7)
        COSIN(1)=1./SQRT(1.+DYDX**2+DZDX**2)
        IF (QUAD.EQ.3.OR.QUAD.EQ.7.OR.QUAD.EQ.11) THEN
          COSIN(1)=-COSIN(1)
        END IF
        COSIN(2)=DYDX*COSIN(1)
        COSIN(3)=DZDX*COSIN(1)
      ELSE IF (MOD(QUAD,2).EQ.0) THEN
        DXDY=CLIST(5)
        DZDY=CLIST(7)
        COSIN(2)=1./SQRT(1.+DXDY**2+DZDY**2)
        IF (QUAD.EQ.4.OR.QUAD.EQ.8.OR.QUAD.EQ.12) THEN
          COSIN(2)=-COSIN(2)
        END IF
        COSIN(1)=DXDY*COSIN(2)
        COSIN(3)=DZDY*COSIN(2)
      END IF
      PHI = ATAN2(COSIN(2),COSIN(1))
      IF (COSIN(2).LT.0.) PHI = PHI + TWOPI
      THETA = ACOS(COSIN(3))
      IF (ABS(COSIN(3)).LT.0.999) ETA=-ALOG(TAN(THETA/2.))
      IQ(LMUON+10)=8   
C           method of fit; the first 7 are used in A. Klatchko's  code
      ELCAL=Q(LMUON+24)
      ELFE=Q(LMUON+25)
      IF (ELFE.GT.20.) ELFE=20.
      IF (ELCAL.GT.20.) ELCAL=20.
      ELOSS=ELFE/2.+ELCAL
      P1=ABS(CLIST(10))
      P1=MAX(P1,0.000001)
      P1=1./P1
      P=P1+ELOSS
      ELOSS=ELCAL+ELFE
      P=MAX(P,ELOSS)
      IF (CLIST(10).GT.0.) THEN
        IQ(LMUON+2)=-14   ! mu+
      ELSE
        IQ(LMUON+2)=14    ! mu-
      END IF
      Q(LMUON+14)=P
      Q(LMUON+11)=P*COSIN(1)
      Q(LMUON+12)=P*COSIN(2)
      Q(LMUON+13)=P*COSIN(3)
      PT=SQRT(Q(LMUON+11)**2+Q(LMUON+12)**2)
      Q(LMUON+15)=PT
      Q(LMUON+16)=THETA
      Q(LMUON+17)=ETA
      Q(LMUON+18)=PHI
      RADLEN=Q(LMUON+20)
      CALL UCOPY2(Q(LMUON+50),COSOUT,3)
      CALL CROSS(COSIN,COSOUT,DIF)
      MCS=0.0136*SQRT(RADLEN)*(1.+0.038*LOG(RADLEN))
      MCS=MCS/(Q(LMUOT+22)+0.001)
      MCS=MCS*CLIST(10)
      DRHO2=CLIST(59)+MCS**2 ! error squared of (1/p)
      DP2=P1**4*DRHO2     ! momentum error squared (MCS error folded in)
      DP=SQRT(DP2)
      DPOP=DP/P
      Q(LMUON+29)=DP2
      Q(LMUON+26)=DP2*COSIN(1)**2
      Q(LMUON+27)=DP2*COSIN(2)**2
      Q(LMUON+28)=DP2*COSIN(3)**2
      Q(LMUON+30)=DP2*(PT/P)**2
  999 RETURN
      END
