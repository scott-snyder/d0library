      SUBROUTINE SA_MFIT_FILL(IMUOT,IMUON,IFAIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill MFIT bank from MUOT and
C-                         MUKF. If there is no MUKF under MUON,
C-                         just copy MUOT to MFIT
C-                         (SAMUS version)
C-
C-   Inputs  :    IMUON   -   # of MUON bank
C-                IMUOT   -   # of MUOT bank
C-   Outputs :    (implicit)  MFIT bank
C-                IFAIL   -   Error code (1 = no error, <=0 error, >1 warning
C-   Controls:
C-
C-   Created  20-JAN-1995     I.Mandrichenko
C-   Updated  04-FEB-1995   I.M. New format of STTH
C-   Updated  17-MAR-1995   I.M. Simplify error calculation
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C----------------------------------------------------------------------
      INTEGER IMUOT,IMUON,IFAIL
C----------------------------------------------------------------------
      INTEGER LMFIT,GZMUON,LMUON,LMUOT,GZMUOT,LMUKF
      INTEGER LSTTH,NFITA,NFITB,NFITC,NABC,DUMMY
      INTEGER NKF,KFNP,KFVER,KFNH
      REAL    KFIT(5),ZKF,CHIKF,KFMTX(5,5),CZ,PMOM,DPFIT,BDL
      INTEGER IMN,NS
C----------------------------------------------------------------------
C
C-------- BOOK & FILL MFIT BANK
C
      IFAIL = -1
      LMUON = GZMUON(IMUON)
      IF(LMUON .LE. 0) GOTO 999
C<<
      CALL GRLINK('SA_MFIT_FILL', IMN)
      LRLINK(IMN) = LMUON
C<<
      CALL BKMFIT(IMN,LMFIT)
      CALL RRLINK('SA_MFIT_FILL', IMN)
C<<
      IFAIL = -2
      LMUON = GZMUON(IMUON)
      IF(LMUON .LE. 0) GOTO 999
C<<
      IFAIL = -3
      LMUOT = GZMUOT(IMUOT)
      IF(LMUOT .LE. 0) GOTO 999
C<<
      IFAIL = -4
      LSTTH = LQ(LMUOT-IZSTTH)
      IF(LSTTH.LE.0) GOTO 999
      NABC = 0
      CALL    GTSTTH(IMUOT,LSTTH,NABC,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY)
      IF(NABC.LT.0) GOTO 999
      NFITA = MOD(NABC,10)
      NFITB = MOD(NABC/10,10)
      NFITC = NABC/100
C<<
      LMUKF = LQ(LMUON-IZMUKF)
      IF( LMUKF.GT.0 ) THEN
        IF( IQ(LMUKF+1).EQ.1 ) THEN
          NKF = IQ(LMUKF+2)
          CALL GTMUKF(LMUKF, IMUON, NKF-1, KFVER, KFNP, KFNH, ZKF,
     +                    CHIKF, BDL, KFIT, KFMTX)
          IFAIL = 2
          IF( KFIT(5).EQ.0. ) LMUKF = 0
          IF( KFNH.LE.5 ) LMUKF = 0
	  IF( KFMTX(1,1).LE.0. ) LMUKF = 0
	  IF( KFMTX(2,2).LE.0. ) LMUKF = 0
	  IF( KFMTX(3,3).LE.0. ) LMUKF = 0
	  IF( KFMTX(4,4).LE.0. ) LMUKF = 0
	  IF( KFMTX(5,5).LE.0. ) LMUKF = 0
        ELSE
          IFAIL = 3
          LMUKF = 0
        END IF
      END IF
C
      NS = IQ(LMFIT - 2)
      LQ(LMFIT - NS - 1) = LMUOT
C<<
      IQ(LMFIT+1) = 1         ! Version
      IQ(LMFIT+2) = 0         ! NWAMUS
      IQ(LMFIT+4) = IQ(LMUOT+3)       ! Quadrant
      IQ(LMFIT+5) = 3         ! 3D fit
      IQ(LMFIT+6) = 1         ! Origin = vertex
      IQ(LMFIT+8) = 0         ! Quality flag (?)
      CALL    UCOPY(Q(LMUOT+11), Q(LMFIT+14), 3)
      CALL    UCOPY(Q(LMUOT+17), Q(LMFIT+17), 3)
      Q(LMFIT+32) = Q(LMUOT+25)
      Q(LMFIT+33) = Q(LMUOT+26)
C<<
      IF(LMUKF.LE.0) THEN
        IQ(LMFIT+3) = NFITA+NFITB+NFITC
        IQ(LMFIT+7) = 3     ! Copy of MUOT
        PMOM = Q(LMUOT+23)
        IQ(LMFIT+10) = SIGN(1.,PMOM)
        CALL    UCOPY(Q(LMUOT+8),  Q(LMFIT+11), 3)
        Q(LMFIT+20) = Q(LMUOT+23)*Q(LMUOT+14)
        Q(LMFIT+21) = Q(LMUOT+23)*Q(LMUOT+15)
        Q(LMFIT+22) = Q(LMUOT+23)*Q(LMUOT+16)
        PMOM = ABS(PMOM)
        Q(LMFIT+23) = PMOM
        Q(LMFIT+24) = SQRT(Q(LMFIT+20)**2 + Q(LMFIT+21)**2)
        DPFIT = ABS(Q(LMUOT + 24) / PMOM)
        Q(LMFIT+30) = Q(LMUOT+20)
        Q(LMFIT+31) = Q(LMUOT+22)
      ELSE
        IQ(LMFIT+3) = NABC
        IQ(LMFIT+7) = 0     ! Kalman fit
        PMOM = 1./KFIT(5)
        IQ(LMFIT+10) = SIGN(1.,PMOM)
        PMOM = ABS(PMOM)
        Q(LMFIT+11) = KFIT(1)
        Q(LMFIT+12) = KFIT(2)
        Q(LMFIT+13) = ZKF
        CZ = 1./SQRT(1.+KFIT(3)**2+KFIT(4)**2)
        IF( IQ(LMFIT+4).EQ.13 ) CZ = -CZ
        Q(LMFIT+22) = PMOM*CZ
        Q(LMFIT+20) = Q(LMFIT+22)*KFIT(3)
        Q(LMFIT+21) = Q(LMFIT+22)*KFIT(4)
        Q(LMFIT+23) = PMOM
        Q(LMFIT+24) = PMOM * SQRT( KFIT(3)**2 + KFIT(4)**2 )
        DPFIT = SQRT(KFMTX(5,5))*PMOM	  ! (Sigma 1/P)*P
        Q(LMFIT+30) = CHIKF/(KFNH-5)
        Q(LMFIT+31) = BDL
      END IF
      Q(LMFIT+25) = (DPFIT * Q(LMFIT+20))**2
      Q(LMFIT+26) = (DPFIT * Q(LMFIT+21))**2
      Q(LMFIT+27) = (DPFIT * Q(LMFIT+22))**2
      Q(LMFIT+28) = (DPFIT * Q(LMFIT+23))**2
      Q(LMFIT+29) = (DPFIT * Q(LMFIT+24))**2
      IFAIL = 1
C<<
  999 RETURN
      END
