      SUBROUTINE GTMDST_PMUO(IMUO,E,ET,SIG,SIGET,THETA,ETA,PHI,CHISQ,
     &  CHISQ_PROB,XYZ,R_ISOLATION,XPCTD_E_LOSS,CAL_E_LOSS,ANGLE,CHARGE,
     &  NDF,METHOD,IFLAG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get PMUO information from MDST bank
C-
C-   Inputs  : IMUO = # of muon desired
C-   Outputs :
C-              E(5) = (PX,PY,PZ,ENERGY)
C-              ET
C-              SIG(I) = error on the Ith E(I)
C-              SIGET
C-              THETA = theta of muon
C-              ETA = eta of muon
C-              PHI = phi of muon
C-              CHISQ = chi**2 of muon track fit
C-              CHISQ_PROB = probability of CHISQ
C-              XYZ = X, Y AND Z WHERE TRACK DEFINED
C-              R_ISOLATION(J) = various isolation values:
C                         J=1 isolation parameter (based on cells hit only)
C                         J=2 isolation parameter (based on cells
C                         hit+neighbors)
C                         J=3 isolation parameter (based on cone size 0.2)
C                         J=4 isolation parameter (based on cone size 0.4)
C                         J=5 isolation parameter (based on cone size 0.6)
C-              XPCTD_E_LOSS = expected energy lost in the calorimeter
C-              CAL_E_LOSS(3) = actual calorimeter energy associated with muon
C-              ANGLE = ANGLE BETWEEN MUON AND NEAREST CD TRACK
C-              CHARGE = charge of muon
C-              NDF = number of degrees of freedom of muon fit
C-              METHOD = i2*100 + i1*10 +i0
C                             where
C                                i0: 1= vertex point is used in fitting
C                                i1: 1= central track is used
C                                i2: 1= E loss in CAL is corrected.
C-              IFLAG = where track vector is defind.
C                                1= vertex point
C                                2= in the muon system.
C                                3= other place
C-   Controls:
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'

      REAL    E(5),SIG(5),THETA,ETA,PHI,CHISQ,CHISQ_PROB,R_ISOLATION(5)
      REAL    XPCTD_E_LOSS,CAL_E_LOSS(3),R_NDF,XYZ(3),ANGLE,ET
      REAL    SIGET
      INTEGER NDF,METHOD,IFLAG,LZMDST,GZMDST,NUM_MUO,IMUO,NREP,IOFF
      INTEGER NUM_PMUO,IER,CHARGE
C----------------------------------------------------------------------
      IER = 0
      E(1)=0.
      E(2)=0.
      E(3)=0.
      E(4)=0.
      ET  =0.
      THETA=0.
      ETA=0.
      PHI=0.
      SIG(1)=0.
      SIG(2)=0.
      SIG(3)=0.
      SIG(4)=0.
      SIGET =0.
      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF

      NUM_MUO=IQ(LZMDST+6)
      IF( IMUO.GT.NUM_MUO) THEN
        IER = -5
        GOTO 999
      ENDIF

      NREP=IQ(LZMDST+5)
      IOFF=IQ(LZMDST+7)+(IMUO-1)*NREP-1

      E(1)=Q(LZMDST+IOFF+1)
      E(2)=Q(LZMDST+IOFF+2)
      E(3)=Q(LZMDST+IOFF+3)
      E(4)=Q(LZMDST+IOFF+4)
      ET  =Q(LZMDST+IOFF+5)
      SIG(1)=Q(LZMDST+IOFF+6)
      SIG(2)=Q(LZMDST+IOFF+7)
      SIG(3)=Q(LZMDST+IOFF+8)
      SIG(4)=Q(LZMDST+IOFF+9)
      SIGET =Q(LZMDST+IOFF+10)

      THETA=Q(LZMDST+IOFF+11)
      ETA=Q(LZMDST+IOFF+12)
      PHI=Q(LZMDST+IOFF+13)

      CHISQ=Q(LZMDST+IOFF+14)
      CHISQ_PROB=Q(LZMDST+IOFF+15)
      XYZ(1) = Q(LZMDST+IOFF+16)
      XYZ(2) = Q(LZMDST+IOFF+17)
      XYZ(3) = Q(LZMDST+IOFF+18)

      R_ISOLATION(1)=Q(LZMDST+IOFF+19)
      R_ISOLATION(2)=Q(LZMDST+IOFF+20)
      R_ISOLATION(3)=Q(LZMDST+IOFF+21)
      R_ISOLATION(4)=Q(LZMDST+IOFF+22)
      R_ISOLATION(5)=Q(LZMDST+IOFF+23)
      XPCTD_E_LOSS=Q(LZMDST+IOFF+24)
      CAL_E_LOSS(1) = Q(LZMDST+IOFF+25)
      CAL_E_LOSS(2) = Q(LZMDST+IOFF+26)
      CAL_E_LOSS(3) = Q(LZMDST+IOFF+27)
      ANGLE         = Q(LZMDST+IOFF+28)
      CHARGE=IQ(LZMDST+IOFF+29)
      IF ( CHARGE.GT.0 ) THEN
        CHARGE=-1
      ELSE
        CHARGE=1
      ENDIF
      NDF=IQ(LZMDST+IOFF+30)
      METHOD=IQ(LZMDST+IOFF+31)
      IFLAG=IQ(LZMDST+IOFF+32)
  999 RETURN

      ENTRY GTMDST_PMUO_TOTAL(NUM_PMUO,IER)
      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        IER = -4
        NUM_PMUO = 0
      ELSE
        NUM_PMUO = IQ(LZMDST+6)
        IER = 0
      ENDIF
      RETURN

      END
