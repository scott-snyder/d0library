      SUBROUTINE MFGLOBAL(LMUON,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Global fitting of a forward muon track 
C-   detected in WAMUS. 
C- 
C-   Inputs  :  LMUON  bank address 
C-   Outputs :  IERR   =0 OK, =1 no ZTRK, =2 no FDCT, =3 NO VTXT) 
C-                    =4 bad WAMUS A layer 
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT
C-
C-
C-   Created   5-SEP-1992   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER GFIT,GFITF,IER,QUAD
      INTEGER IMUON,LMUON,GZMUON,LMUOT,LVERT,LZTRK,LFDCT,IERR 
      REAL CLIST(75),P,DXDZ,DYDZ,COSIN(3),COSOUT(3)
      REAL ZC 
      REAL Z0,P1,ELFE,ELCAL,ELOSS,RADLEN,MCS,DP2,DP,CHICUT
      REAL PHI,THETA,ETA,PT,DIF(3),BEND,DPOP,DRHO2
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        CALL EZPICK('MURECO_RCP')
        FIRST=.FALSE.
        CALL EZGET('CHI_MIN_CD',CHICUT,IER)
        CALL EZGET('GFIT',GFIT,IER)
        CALL EZGET('GFITF',GFITF,IER)
        CALL EZRSET
      END IF
      IERR=0
      LMUOT=LQ(LMUON-11)
      QUAD=IQ(LMUOT+3)
      IF (QUAD.LE.4) GO TO 999  
      IF (GFITF.NE.1) GO TO 999  
      IF (IQ(LMUOT+4).EQ.5) GO TO 999 ! A layer stub  
      LVERT=LQ(LMUON-12)
      IF (LVERT.LE.0) THEN
        GO TO 999
      END IF
      IMUON=IQ(LMUON-5)
      CALL MULOFIT(IMUON)    ! make MSEG banks for this track
      LMUON=GZMUON(IMUON)
      IF (IQ(LMUON+4).EQ.11.OR.IQ(LMUON+4).EQ.110) THEN
        GO TO 999
      END IF
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.GT.0) THEN
        LFDCT=LQ(LZTRK-8)
        IF (LFDCT.GT.0) THEN
          CALL MUFITFABC(LMUON,CLIST,Z0,ZC,IERR)  
          IQ(LMUON+4)=2 
          IF (CLIST(1).GT.1..AND.IERR.LE.4) THEN
            Q(LMUON+31)=CLIST(3)/CLIST(2)  ! chisq/df 
            IF (Q(LMUON+31).LT.CHICUT) THEN
              IQ(LMUON+4)=11   ! CD and vertex used 
            END IF
          END IF
        END IF
      END IF
      IF (IQ(LMUON+4).EQ.0.OR.IQ(LMUON+4).EQ.2) THEN      
        IF (QUAD.GT.4) THEN
          CALL MUFITEFCAL(LMUON,CLIST,IERR)  ! no CD; use MTC 
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
      DXDZ=CLIST(5)                    ! slopes at production
      DYDZ=CLIST(7)              
      COSIN(3)=1./SQRT(1.+DXDZ**2+DYDZ**2)
      COSIN(3)=SIGN(COSIN(3),Q(LMUON+52))
      COSIN(1)=DXDZ*COSIN(3)
      COSIN(2)=DYDZ*COSIN(3)
      PHI = ATAN2(COSIN(2),COSIN(1))
      IF (COSIN(2).LT.0.) PHI = PHI + TWOPI
      THETA = ACOS(COSIN(3))
      IF (ABS(COSIN(3)).LT.0.999) ETA=-ALOG(TAN(THETA/2.))
      IQ(LMUON+10)=8  
C            method if fit; the first 7 are used in A. Klatchko's code
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
      BEND=SQRT(DIF(1)**2+DIF(2)**2+DIF(3)**2)
      BEND=MAX(BEND,0.000001)
      MCS=0.0136*SQRT(RADLEN)*(1.+0.038*LOG(RADLEN))
      MCS=MCS/(Q(LMUOT+22)+0.001)
      MCS=MCS*CLIST(10) 
      DRHO2=CLIST(59)+MCS**2 ! error squared of (1/p)
      DP2=P1**4*DRHO2     ! momentum error squared (MCS error folded in)
      DP=SQRT(DP2)
      DPOP=DP/P
      Q(LMUON+26)=DP2*COSIN(1)**2
      Q(LMUON+27)=DP2*COSIN(2)**2
      Q(LMUON+28)=DP2*COSIN(3)**2
      Q(LMUON+29)=DP2
      Q(LMUON+30)=DP2*(PT/P)**2
  999 RETURN
      END
